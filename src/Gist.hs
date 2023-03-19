module Gist
  ( Gist(..)
  , Configurable(..)
  , Config(..)
  , Prettily(..)
  , config
  , strConfig
  , gist
  , ConfMap(..)
  , ConfStrQuotes(..)
  ) where

import           Data.Bifunctor                 ( first )
import qualified Data.Char                     as Char
import qualified Data.Dynamic                  as Dyn
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Kind                      ( Type )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Last(..) )
import           Data.String                    ( IsString )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           GHC.Stack                      ( HasCallStack )
import           Prettyprinter
import qualified Text.Printf                   as Printf
import           Text.Read                      ( readMaybe )
import           Type.Reflection                ( SomeTypeRep(..)
                                                , TypeRep
                                                , Typeable
                                                , typeRep
                                                )

-- | A gist is a configurable pretty-print.
class Typeable a => Gist a where
  {-# MINIMAL configFor, (gist' | gistPrec') #-}

  type ConfigFor a :: Type
  configFor :: Config -> ConfigFor a

  gistPrec' :: Int -> ConfigFor a -> a -> Doc ann
  gistPrec' _ = gist'

  gist' :: ConfigFor a -> a -> Doc ann
  gist' = gistPrec' 0

  type ConfigForList a :: Type
  type ConfigForList a = (Maybe Int, Config)

  configForList :: Config -> ConfigForList a
  default configForList
    :: ( Configurable a
       , ConfigForList a ~ (Maybe Int, Config)
       , WriteConfigForList a ~ (Last Int, Last Config)
       )
    => Config
    -> ConfigForList a
  configForList conf =
    let (Last mTake, Last mSubConf) = configLookups @( [] :&& [a] ) conf
    in (mTake, fromMaybe conf mSubConf)

  gistList' :: ConfigForList a -> [a] -> Doc ann
  default gistList'
    :: (ConfigForList a ~ (Maybe Int, Config))
    => ConfigForList a
    -> [a]
    -> Doc ann
  gistList' (mTake, subConf) l =
    let subConfFor = configFor @a subConf
        elems = case mTake of
          Nothing -> map (gist' subConfFor) l
          Just n  -> case splitAt n l of
            (start, []   ) -> map (gist' subConfFor) start
            (start, _ : _) -> map (gist' subConfFor) start ++ ["..."]
    in align $ list elems

-- | This describes the type of the configuration for a gist.
--
-- Some things are `Configurable` but not `Gist`. In particular, types of kind
-- other than `Type` can be `Configurable`, and gists can look up the config for
-- those types too. So we can have one configuration for `Floating` and a more
-- specific one for `Double`; or one for `[]` and a more specific one for
-- `[()]`.
class
  ( Typeable a
  , Monoid (WriteConfigFor a)
  , Typeable (WriteConfigFor a)
  , Monoid (WriteConfigForList a)
  , Typeable (WriteConfigForList a)
  ) => Configurable a
 where
  type WriteConfigFor a :: Type
  type WriteConfigFor a = ()

  parseConfigFor :: String -> Either String (WriteConfigFor a)
  default parseConfigFor
    :: WriteConfigFor a ~ () => String -> Either String (WriteConfigFor a)
  parseConfigFor _ = Left "Not configurable"

  type WriteConfigForList a :: Type
  type WriteConfigForList a = WriteConfigFor []

  parseConfigForList :: String -> Either String (WriteConfigForList a)

  -- If we replace this constrant with
  --
  --     WriteConfigForList a ~ WriteConfigFor
  --
  -- we get compile failures for some reason.
  default parseConfigForList
    :: WriteConfigForList a ~ (Last Int, Last Config)
    => String
    -> Either String (WriteConfigForList a)
  parseConfigForList = parseConfigFor @[]

data SomeConfigurable = forall a . Configurable a => SomeConfigurable
                                                       !(TypeRep a)

instance Show SomeConfigurable where
  showsPrec p (SomeConfigurable tr) = showsPrec p tr

instance Eq SomeConfigurable where
  SomeConfigurable a == SomeConfigurable b = SomeTypeRep a == SomeTypeRep b

instance Ord SomeConfigurable where
  SomeConfigurable a `compare` SomeConfigurable b =
    SomeTypeRep a `compare` SomeTypeRep b

newtype Config =
  UnsafeConfig { unsafeUnConfig :: Map SomeConfigurable Dyn.Dynamic }

instance Semigroup Config where
  UnsafeConfig m1 <> UnsafeConfig m2 = UnsafeConfig $ Map.unionWithKey f m1 m2
   where
    f sc dyn1 dyn2 = case sc of
      SomeConfigurable c -> concatDyns c dyn1 dyn2

    concatDyns
      :: forall a
       . Configurable a
      => TypeRep a
      -> Dyn.Dynamic
      -> Dyn.Dynamic
      -> Dyn.Dynamic
    concatDyns _ dyn1 dyn2 =
      case
          ( Dyn.fromDynamic @(WriteConfigFor a) dyn1
          , Dyn.fromDynamic @(WriteConfigFor a) dyn2
          )
        of
          (Just c1, Just c2) -> Dyn.toDyn $ c1 <> c2
          _                  -> error "Bad Dynamic saved in Config"

instance Monoid Config where
  mempty = UnsafeConfig mempty

configInsert
  :: forall a . Configurable a => WriteConfigFor a -> Config -> Config
configInsert confFor (UnsafeConfig m) = UnsafeConfig
  $ Map.insert (SomeConfigurable $ typeRep @a) (Dyn.toDyn confFor) m

configLookup :: forall a . Configurable a => Config -> WriteConfigFor a
configLookup (UnsafeConfig m) =
  case Map.lookup (SomeConfigurable $ typeRep @a) m of
    Nothing  -> mempty
    Just dyn -> case Dyn.fromDynamic dyn of
      Nothing   -> error "Bad Dynamic saved in Config"
      Just conf -> conf

-- We use `:&` and `:&&` for `configLookups`. We can't use type-level lists
-- `@'[a, b]` because those are hetero-kinded, e.g. `'[ [], [Int] ]` is
-- forbidden. We need them as separate types because otherwise we'd have
-- overlapping instances
--
--     instance (Configurable a, Configurable b) => ConfigLookups (a :& b)
--     instance (ConfigLookups a, Configurable b) => ConfigLookups (a :& b)

data (:&) a b -- brittany doesn't like `a :& b`.
infixl 5 :&
data (:&&) a b -- brittany doesn't like `a :&& b`.
infix 6 :&&

-- | We can use this class to lookup configs for multiple types at once,
-- provided they all have the same `WriteConfigFor`. Replace
--
--     configLookup @a conf
--       <> configLookup @b conf
--       <> configLookup @c conf
--       <> ...
--
-- with
--
--     configLookups @(a :&& b :& c :& ...) conf
class ConfigLookups as where
  type ConfigLookupsResult as
  configLookups :: Config -> ConfigLookupsResult as

instance (Configurable a, Configurable b, WriteConfigFor a ~ WriteConfigFor b)
  => ConfigLookups (a :&& b)
 where
  type ConfigLookupsResult (a :&& b) = WriteConfigFor a
  configLookups conf = configLookup @a conf <> configLookup @b conf

instance
  ( Configurable a
  , ConfigLookups as
  , WriteConfigFor a ~ ConfigLookupsResult as
  ) => ConfigLookups (as :& a)
 where
  type ConfigLookupsResult (as :& a) = WriteConfigFor a
  configLookups conf = configLookups @as conf <> configLookup @a conf

-- | For types with a `Pretty` instance, you can derive `Gist` and
-- `Configurable` via `Prettily`. The resulting instances will have no
-- configuration.
newtype Prettily a = Prettily a

instance (Configurable a, Pretty a, WriteConfigFor a ~ ()) => Gist (Prettily a) where
  type ConfigFor (Prettily a) = ()
  configFor _ = ()
  gistPrec' _ ~() (Prettily a) = pretty a

instance Typeable a => Configurable (Prettily a)

-- | Concatenates several configs into one before applying.
--
-- This lets us do `gist [strConfig ..., config ...]`. But maybe we want an
-- `IsConfig` typeclass.
gist :: forall a ann . Gist a => [Config] -> a -> Doc ann
gist confs = gist' $ configFor @a $ mconcat confs

-- | Parse a `Config` from a string.
strConfig :: forall a . (HasCallStack, Configurable a) => String -> Config
strConfig s = case parseConfigFor @a s of
  Left  err     -> error $ "Could not parse config: " <> err
  Right confFor -> config @a confFor

-- | Create a singleton `Config`.
config :: forall a . Configurable a => WriteConfigFor a -> Config
config confFor = configInsert @a confFor mempty

-- Most `Gist` instances don't explicitly need a `Configurable` instance, but
-- this one does. e.g. we have
--
--     instance (Gist k, Gist v) => Gist (Map k v)
--     instance (Configurable a, Gist a) => Gist [a]
--
-- The difference is that the `Gist (Map k v)` instance does need `Configurable
-- (Map k v)`, but that only depends on `(Typeable k, Typeable v)`, which are
-- provided by `(Gist k, Gist v)`. Meanwhile the `Configurable [a]` instance
-- depends on `Configurable a`, because of `WriteConfigForList` and friends. So
-- we can't deduce `Configurable [a]` simply from `Gist a`, whereas we can
-- deduce `Configurable (Map k v)` from `(Gist k, Gist v)`.
instance (Configurable a, Gist a) => Gist [a] where
  type ConfigFor [a] = ConfigForList a
  configFor = configForList @a
  gistPrec' _ = gistList' @a

instance Configurable a => Configurable [a] where
  type WriteConfigFor [a] = WriteConfigForList a
  parseConfigFor = parseConfigForList @a

instance Configurable [] where
  type WriteConfigFor [] = (Last Int, Last Config)
  parseConfigFor s = case words s of
    ["show-first", n] -> case readMaybe n of
      Just n' -> Right (pure n', mempty)
      Nothing -> Left "Expected \"show-first (int)\""
    _ -> Left "Expected \"show-first (int)\""

deriving via (Prettily ()) instance Gist ()
deriving via (Prettily ()) instance Configurable ()

-- | TODO: allow comma and underscore separation.
deriving via (Prettily Int) instance Gist Int
deriving via (Prettily Int) instance Configurable Int

instance Gist Float where
  type ConfigFor Float = Maybe Printf.FieldFormat
  configFor conf = getLast $ configLookups @(Floating :&& Float) conf
  gistPrec' _ = \case
    Nothing -> pretty
    Just f  -> \d -> pretty $ Printf.formatRealFloat d f ""

instance Configurable Float where
  type WriteConfigFor Float = WriteConfigFor Floating
  parseConfigFor = parseConfigFor @Floating

instance Gist Double where
  type ConfigFor Double = Maybe Printf.FieldFormat
  configFor conf = getLast $ configLookups @(Floating :&& Double) conf
  gistPrec' _ = \case
    Nothing -> pretty
    Just f  -> \d -> pretty $ Printf.formatRealFloat d f ""

instance Configurable Double where
  type WriteConfigFor Double = WriteConfigFor Floating
  parseConfigFor = parseConfigFor @Floating

-- | TODO: allow comma and underscore separation. Also, there's no way to revert
-- to the default behavior. And there's no instance for `Show FieldFormat`.
instance Configurable Floating where
  type WriteConfigFor Floating = Last Printf.FieldFormat
  parseConfigFor = go
   where
    go = \case
      [] -> Left "incomplete format string"
      ('-' : s) ->
        go s <&&> \f -> f { Printf.fmtAdjust = Just Printf.LeftAdjust }
      ('+' : s) -> go s <&&> \f -> f { Printf.fmtSign = Just Printf.SignPlus }
      (' ' : s) -> go s <&&> \f -> f { Printf.fmtSign = Just Printf.SignSpace }
      ('0' : s) -> go s <&&> \f -> f { Printf.fmtAdjust = Just Printf.ZeroPad }
      ('#' : s) -> go s <&&> \f -> f { Printf.fmtAlternate = True }
      s         -> do
        let isDigit         = (`elem` ("0123456789" :: String))
            isFmtChar       = (`elem` ("fFgGeE" :: String))
            (widthS, rest1) = span isDigit s
        widthI <- if null widthS
          then Right Nothing
          else maybe (Left "cannot parse width")
                     (Right . Just)
                     (readMaybe widthS)
        let (mPrecS, rest2) = case rest1 of
              ('.' : s') -> first Just $ span isDigit s'
              _          -> (Nothing, rest1)
        precI <- case mPrecS of
          Nothing    -> Right Nothing
          Just ""    -> Right (Just 0)
          Just precS -> maybe (Left "cannot parse precision")
                              (Right . Just)
                              (readMaybe precS)
        case rest2 of
          (c : []) | isFmtChar c -> Right $ pure $ Printf.FieldFormat
            { Printf.fmtWidth     = widthI
            , Printf.fmtPrecision = precI
            , Printf.fmtAdjust    = Nothing
            , Printf.fmtSign      = Nothing
            , Printf.fmtAlternate = False
            , Printf.fmtModifiers = ""
            , Printf.fmtChar      = c
            }
          _ -> Left "cannot parse format specifier"

    a <&&> f = fmap (fmap f) a

data ConfStrQuotes
  = ConfStrQuotesAlways
  | ConfStrQuotesNever
  | ConfStrQuotesSometimes
  deriving stock (Eq, Show)

-- | If we use `ConfStrQuotesSometimes`, this decides whether an individual
-- character should be quoted. A string is quoted if it contains any characters
-- that should be quoted.
--
-- Alphanumeric chars, dashes and connectors are unquoted, including ones
-- outside the ASCII range. Notably this includes `-` and `_`. Everything else
-- is quoted.
charWantsQuotes :: Char -> Bool
charWantsQuotes c =
  not (Char.isAlphaNum c)
    && (         Char.generalCategory c
       `notElem` [Char.ConnectorPunctuation, Char.DashPunctuation]
       )

-- | TODO: allow (default to?) C-style escaping or similar.
instance Configurable IsString where
  type WriteConfigFor IsString = Last ConfStrQuotes
  parseConfigFor = \case
    "quotes-always"    -> Right $ pure ConfStrQuotesAlways
    "quotes-never"     -> Right $ pure ConfStrQuotesNever
    "quotes-sometimes" -> Right $ pure ConfStrQuotesSometimes
    _                  -> Left "unknown quote specifier"

instance Gist Char where
  type ConfigFor Char = ConfStrQuotes
  configFor =
    fromLast ConfStrQuotesSometimes . configLookups @(IsString :&& Char)
  gistPrec' _ conf c = case conf of
    ConfStrQuotesAlways    -> viaShow c
    ConfStrQuotesNever     -> pretty c
    ConfStrQuotesSometimes -> if charWantsQuotes c then viaShow c else pretty c

  type ConfigForList Char = ConfStrQuotes
  configForList =
    fromLast ConfStrQuotesSometimes . configLookups @(IsString :&& String)
  gistList' conf s = gist' @Text conf (Text.pack s)

instance Configurable Char where
  type WriteConfigFor Char = WriteConfigFor IsString
  parseConfigFor = parseConfigFor @IsString

  type WriteConfigForList Char = Last ConfStrQuotes
  parseConfigForList = parseConfigFor @Char

instance Gist Text where
  type ConfigFor Text = ConfStrQuotes
  configFor =
    fromLast ConfStrQuotesSometimes . configLookups @(IsString :&& Text)
  gistPrec' _ conf s = case conf of
    ConfStrQuotesAlways -> viaShow s
    ConfStrQuotesNever  -> pretty s
    ConfStrQuotesSometimes ->
      if Text.any charWantsQuotes s then viaShow s else pretty s

instance Configurable Text where
  type WriteConfigFor Text = WriteConfigFor String
  parseConfigFor = parseConfigFor @String

instance (Gist a, Gist b) => Gist (a, b) where
  type ConfigFor (a, b) = (Config, Config)
  configFor conf = (fromLast conf confA, fromLast conf confB)
    where (confA, confB) = configLookups @((,) :&& ((,) a) :& (a, b)) conf
  gistPrec' _ (confA, confB) (a, b) =
    tupled [gist' (configFor @a confA) a, gist' (configFor @b confB) b]

instance (Typeable a, Typeable b) => Configurable (a, b) where
  type WriteConfigFor (a, b) = (Last Config, Last Config)
  parseConfigFor _ = Left "Cannot parse config for (a, b)"

instance Configurable (,) where
  type WriteConfigFor (,) = (Last Config, Last Config)
  parseConfigFor _ = Left "Cannot parse config for (,)"

instance Typeable a => Configurable ((,) a) where
  type WriteConfigFor ((,) a) = (Last Config, Last Config)
  parseConfigFor _ = Left "Cannot parse config for ((,) a)"

data ConfMap f = ConfMap
  { confMapShowKeys :: f Bool
  , confMapShowVals :: f Bool
  }

deriving stock instance Eq (f Bool) => Eq (ConfMap f)
deriving stock instance Show (f Bool) => Show (ConfMap f)

instance Semigroup (f Bool) => Semigroup (ConfMap f) where
  (ConfMap a1 b1) <> (ConfMap a2 b2) = ConfMap (a1 <> a2) (b1 <> b2)

instance Monoid (f Bool) => Monoid (ConfMap f) where
  mempty = ConfMap mempty mempty

instance (Gist k, Gist v) => Gist (Map k v) where
  type ConfigFor (Map k v) = (ConfMap Identity, Config, Config)

  configFor conf =
    ( ConfMap (Identity $ fromLast True showKeys)
              (Identity $ fromLast True showVals)
    , fromLast conf confK
    , fromLast conf confV
    )
   where
    (ConfMap showKeys showVals, confK, confV) =
      configLookups @(Map :&& Map k :& Map k v) conf

  gistPrec' _ (ConfMap showKeys showVals, confK, confV) m =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ (showKV <$> Map.toList m)
   where
    showKV (k, v) =
      (if runIdentity showKeys then gist' (configFor @k confK) k else "_")
        <> ": "
        <> (if runIdentity showVals then gist' (configFor @v confV) v else "_")

instance (Typeable k, Typeable v) => Configurable (Map k v) where
  type WriteConfigFor (Map k v) = WriteConfigFor Map
  parseConfigFor = parseConfigFor @Map

instance Typeable k => Configurable (Map k) where
  type WriteConfigFor (Map k) = WriteConfigFor Map
  parseConfigFor = parseConfigFor @Map

instance Configurable Map where
  type WriteConfigFor Map = (ConfMap Last, Last Config, Last Config)
  parseConfigFor = \case
    "hide-keys" -> Right (ConfMap (pure False) mempty, mempty, mempty)
    "hide-vals" -> Right (ConfMap mempty (pure False), mempty, mempty)
    _           -> Left "Expected hide-keys or hide-vals"

instance (Gist a, Gist b) => Gist (Either a b) where
  type ConfigFor (Either a b) = (Config, Config)

  configFor conf = (fromLast conf confL, fromLast conf confR)
   where
    (confL, confR) = configLookups @(Either :&& Either a :& Either a b) conf

  gistPrec' prec (confL, confR) = parensIfPrecGT 10 prec . \case
    Left  a -> "Left" <+> gistPrec' 11 (configFor @a confL) a
    Right a -> "Right" <+> gistPrec' 11 (configFor @b confR) a

instance (Typeable a, Typeable b) => Configurable (Either a b) where
  type WriteConfigFor (Either a b) = WriteConfigFor Either
  parseConfigFor = parseConfigFor @Either

instance Typeable a => Configurable (Either a) where
  type WriteConfigFor (Either a) = WriteConfigFor Either
  parseConfigFor = parseConfigFor @Either

instance Configurable Either where
  type WriteConfigFor Either = (Last Config, Last Config)
  parseConfigFor _ = Left "Cannot parse config for Either"

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x

parensIfPrecGT :: Int -> Int -> Doc ann -> Doc ann
parensIfPrecGT comparison prec = if prec > comparison then parens else id
