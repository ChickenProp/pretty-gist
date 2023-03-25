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
import           Data.Functor                   ( (<&>) )
import           Data.Kind                      ( Type )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
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
class Configurable a => Gist a where
  {-# MINIMAL gist' | gistPrec' #-}

  gistPrec' :: Int -> Config -> a -> Doc ann
  gistPrec' _ = gist'

  gist' :: Config -> a -> Doc ann
  gist' = gistPrec' 0

  gistList' :: Config -> [a] -> Doc ann
  default gistList'
    :: (ConfigForList a ~ (Last (Maybe Int), Last Config))
    => Config
    -> [a]
    -> Doc ann
  gistList' conf l =
    let (lTake, lSubConf) =
          configLookups @( [] :&& [a] ) conf
        subConf = fromLast conf lSubConf
        elems = case fromLast Nothing lTake of
          Nothing -> map (gist' subConf) l
          Just n  -> case splitAt n l of
            (start, []   ) -> map (gist' subConf) start
            (start, _ : _) -> map (gist' subConf) start ++ ["..."]
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
  , Monoid (ConfigFor a)
  , Typeable (ConfigFor a)
  , Monoid (ConfigForList a)
  , Typeable (ConfigForList a)
  ) => Configurable a
 where
  type ConfigFor a :: Type
  type ConfigFor a = ()

  parseConfigFor :: String -> Either String (ConfigFor a)
  default parseConfigFor
    :: ConfigFor a ~ () => String -> Either String (ConfigFor a)
  parseConfigFor _ = Left "Not configurable"

  type ConfigForList a :: Type
  type ConfigForList a = ConfigFor []

  parseConfigForList :: String -> Either String (ConfigForList a)

  -- If we replace this constrant with `ConfigForList a ~ ConfigFor []`, we get
  -- compile failures for some reason.
  default parseConfigForList
    :: ConfigForList a ~ (Last (Maybe Int), Last Config)
    => String
    -> Either String (ConfigForList a)
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

deriving via (Showily SomeConfigurable) instance Gist SomeConfigurable
deriving via (Showily SomeConfigurable) instance Configurable SomeConfigurable

deriving via (Showily Dyn.Dynamic) instance Gist Dyn.Dynamic
deriving via (Showily Dyn.Dynamic) instance Configurable Dyn.Dynamic

newtype Config =
  UnsafeConfig { unsafeUnConfig :: Map SomeConfigurable Dyn.Dynamic }
  deriving stock Show

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
          ( Dyn.fromDynamic @(ConfigFor a) dyn1
          , Dyn.fromDynamic @(ConfigFor a) dyn2
          )
        of
          (Just c1, Just c2) -> Dyn.toDyn $ c1 <> c2
          _                  -> error "Bad Dynamic saved in Config"

instance Monoid Config where
  mempty = UnsafeConfig mempty

instance Gist Config where
  gistPrec' prec conf (UnsafeConfig m) = gistPrec' prec conf m

instance Configurable Config

configInsert :: forall a . Configurable a => ConfigFor a -> Config -> Config
configInsert confFor (UnsafeConfig m) = UnsafeConfig
  $ Map.insert (SomeConfigurable $ typeRep @a) (Dyn.toDyn confFor) m

configLookup :: forall a . Configurable a => Config -> ConfigFor a
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
-- provided they all have the same `ConfigFor`. Replace
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

instance (Configurable a, Configurable b, ConfigFor a ~ ConfigFor b)
  => ConfigLookups (a :&& b)
 where
  type ConfigLookupsResult (a :&& b) = ConfigFor a
  configLookups conf = configLookup @a conf <> configLookup @b conf

instance
  ( Configurable a
  , ConfigLookups as
  , ConfigFor a ~ ConfigLookupsResult as
  ) => ConfigLookups (as :& a)
 where
  type ConfigLookupsResult (as :& a) = ConfigFor a
  configLookups conf = configLookups @as conf <> configLookup @a conf

-- | For types with a `Pretty` instance, you can derive `Gist` and
-- `Configurable` via `Prettily`. The resulting instances will have no
-- configuration.
newtype Prettily a = Prettily a

instance (Configurable a, Pretty a, ConfigFor a ~ ()) => Gist (Prettily a) where
  gistPrec' _ _ (Prettily a) = pretty a

instance Typeable a => Configurable (Prettily a)

-- | For types with a `Show` instance, you can derive `Gist` and `Configurable`
-- via `Showily`. The resulting instances will have no configuration.
newtype Showily a = Showily a

instance (Configurable a, Show a, ConfigFor a ~ ()) => Gist (Showily a) where
  gistPrec' prec _ (Showily a) = pretty $ showsPrec prec a ""

instance Typeable a => Configurable (Showily a)

-- | Concatenates several configs into one before applying.
--
-- This lets us do `gist [strConfig ..., config ...]`. But maybe we want an
-- `IsConfig` typeclass.
gist :: Gist a => [Config] -> a -> Doc ann
gist confs = gist' (mconcat confs)

-- | Parse a `Config` from a string.
strConfig :: forall a . (HasCallStack, Configurable a) => String -> Config
strConfig s = case parseConfigFor @a s of
  Left  err     -> error $ "Could not parse config: " <> err
  Right confFor -> config @a confFor

-- | Create a singleton `Config`.
config :: forall a . Configurable a => ConfigFor a -> Config
config confFor = configInsert @a confFor mempty

instance Gist a => Gist [a] where
  gistPrec' _ = gistList' @a

instance Configurable a => Configurable [a] where
  type ConfigFor [a] = ConfigForList a
  parseConfigFor = parseConfigForList @a

-- | TODO: no way in strConfig to reset to default behavior.
instance Configurable [] where
  type ConfigFor [] = (Last (Maybe Int), Last Config)
  parseConfigFor s = case words s of
    ["show-first", "-"] -> Right (pure Nothing, mempty)
    ["show-first", n] -> case readMaybe n of
      Just n' -> Right (pure (Just n'), mempty)
      Nothing -> Left "Expected \"show-first (int | '-')\""
    _ -> Left "Expected \"show-first (int | '-')\""

instance Gist a => Gist (Maybe a) where
  gistPrec' prec conf val = if fromLast False showConstructors
    then case val of
      Nothing -> "Nothing"
      Just v  -> parensIfPrecGT 10 prec $ "Just" <+> gistPrec' 11 subConf v
    else case val of
      Nothing -> "_"
      Just v  -> gistPrec' prec subConf v
   where
    (showConstructors, lSubConf) = configLookups @(Maybe :&& Maybe a) conf
    subConf                      = fromLast conf lSubConf

instance Typeable a => Configurable (Maybe a) where
  type ConfigFor (Maybe a) = ConfigFor Maybe
  parseConfigFor = parseConfigFor @Maybe

instance Configurable Maybe where
  type ConfigFor Maybe = (Last Bool, Last Config)
  parseConfigFor s = case s of
    "show-constructors" -> Right (pure True, mempty)
    "hide-constructors" -> Right (pure False, mempty)
    _ -> Left "Expected \"show-constructors\" or \"hide-constructors\""

deriving via (Prettily ()) instance Gist ()
deriving via (Prettily ()) instance Configurable ()

-- | TODO: allow comma and underscore separation.
deriving via (Prettily Int) instance Gist Int
deriving via (Prettily Int) instance Configurable Int

instance Gist Float where
  gistPrec' _ conf =
    case fromLast Nothing $ configLookups @(Floating :&& Float) conf of
      Nothing -> pretty
      Just f  -> \d -> pretty $ Printf.formatRealFloat d f ""

instance Configurable Float where
  type ConfigFor Float = ConfigFor Floating
  parseConfigFor = parseConfigFor @Floating

instance Gist Double where
  gistPrec' _ conf =
    case fromLast Nothing $ configLookups @(Floating :&& Double) conf of
      Nothing -> pretty
      Just f  -> \d -> pretty $ Printf.formatRealFloat d f ""

instance Configurable Double where
  type ConfigFor Double = ConfigFor Floating
  parseConfigFor = parseConfigFor @Floating

-- | TODO: allow comma and underscore separation. Also, there's no way for
-- strConfig to revert to the default behavior. And there's no instance for
-- `Show FieldFormat`.
instance Configurable Floating where
  type ConfigFor Floating = Last (Maybe Printf.FieldFormat)
  parseConfigFor str = pure <$> case str of
    "-"     -> Right Nothing
    '%' : s -> Just <$> go s
    _       -> Left "Expected '-' or '%'"
   where
    go :: String -> Either String Printf.FieldFormat
    go = \case
      [] -> Left "incomplete format string"
      ('-' : s) ->
        go s <&> \f -> f { Printf.fmtAdjust = Just Printf.LeftAdjust }
      ('+' : s) -> go s <&> \f -> f { Printf.fmtSign = Just Printf.SignPlus }
      (' ' : s) -> go s <&> \f -> f { Printf.fmtSign = Just Printf.SignSpace }
      ('0' : s) -> go s <&> \f -> f { Printf.fmtAdjust = Just Printf.ZeroPad }
      ('#' : s) -> go s <&> \f -> f { Printf.fmtAlternate = True }
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
          (c : []) | isFmtChar c -> Right $ Printf.FieldFormat
            { Printf.fmtWidth     = widthI
            , Printf.fmtPrecision = precI
            , Printf.fmtAdjust    = Nothing
            , Printf.fmtSign      = Nothing
            , Printf.fmtAlternate = False
            , Printf.fmtModifiers = ""
            , Printf.fmtChar      = c
            }
          _ -> Left "cannot parse format specifier"

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
  type ConfigFor IsString = Last ConfStrQuotes
  parseConfigFor = \case
    "quotes-always"    -> Right $ pure ConfStrQuotesAlways
    "quotes-never"     -> Right $ pure ConfStrQuotesNever
    "quotes-sometimes" -> Right $ pure ConfStrQuotesSometimes
    _                  -> Left "unknown quote specifier"

instance Gist Char where
  gistPrec' _ conf c =
    case
        fromLast ConfStrQuotesSometimes
          $ configLookups @(IsString :&& Char) conf
      of
        ConfStrQuotesAlways -> viaShow c
        ConfStrQuotesNever  -> pretty c
        ConfStrQuotesSometimes ->
          if charWantsQuotes c then viaShow c else pretty c

  gistList' conf s =
    case
        fromLast ConfStrQuotesSometimes
          $ configLookups @(IsString :&& String) conf
      of
        ConfStrQuotesAlways -> viaShow s
        ConfStrQuotesNever  -> pretty s
        ConfStrQuotesSometimes ->
          if any charWantsQuotes s then viaShow s else pretty s

instance Configurable Char where
  type ConfigFor Char = ConfigFor IsString
  parseConfigFor = parseConfigFor @IsString

  type ConfigForList Char = Last ConfStrQuotes
  parseConfigForList = parseConfigFor @Char

instance Gist Text where
  gistPrec' _ conf s = gist' (configInsert @String myConf conf) (Text.unpack s)
    where myConf = configLookups @(IsString :&& Text) conf

instance Configurable Text where
  type ConfigFor Text = ConfigFor String
  parseConfigFor = parseConfigFor @String

instance (Gist a, Gist b) => Gist (a, b) where
  gistPrec' _ conf (a, b) = tupled
    [gist' (fromLast conf confA) a, gist' (fromLast conf confB) b]
    where (confA, confB) = configLookups @((,) :&& ((,) a) :& (a, b)) conf

instance (Typeable a, Typeable b) => Configurable (a, b) where
  type ConfigFor (a, b) = (Last Config, Last Config)
  parseConfigFor _ = Left "Cannot parse config for (a, b)"

instance Configurable (,) where
  type ConfigFor (,) = (Last Config, Last Config)
  parseConfigFor _ = Left "Cannot parse config for (,)"

instance Typeable a => Configurable ((,) a) where
  type ConfigFor ((,) a) = (Last Config, Last Config)
  parseConfigFor _ = Left "Cannot parse config for ((,) a)"

data ConfMap = ConfMap
  { confMapShowKeys :: Last Bool
  , confMapShowVals :: Last Bool
  }
  deriving stock (Eq, Show)

instance Semigroup ConfMap where
  (ConfMap a1 b1) <> (ConfMap a2 b2) = ConfMap (a1 <> a2) (b1 <> b2)
instance Monoid ConfMap where
  mempty = ConfMap mempty mempty

instance (Gist k, Gist v) => Gist (Map k v) where
  gistPrec' _ conf m =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ (showKV <$> Map.toList m)
   where
    (ConfMap showKeys showVals, confK, confV) =
      configLookups @(Map :&& Map k :& Map k v) conf
    showKV (k, v) =
      (if fromLast True showKeys then gist' (fromLast conf confK) k else "_")
        <> ": "
        <> (if fromLast True showVals
             then gist' (fromLast conf confV) v
             else "_"
           )

instance (Typeable k, Typeable v) => Configurable (Map k v) where
  type ConfigFor (Map k v) = ConfigFor Map
  parseConfigFor = parseConfigFor @Map

instance Typeable k => Configurable (Map k) where
  type ConfigFor (Map k) = ConfigFor Map
  parseConfigFor = parseConfigFor @Map

instance Configurable Map where
  type ConfigFor Map = (ConfMap, Last Config, Last Config)
  parseConfigFor = \case
    "hide-keys" -> Right (ConfMap (pure False) mempty, mempty, mempty)
    "hide-vals" -> Right (ConfMap mempty (pure False), mempty, mempty)
    _           -> Left "Expected hide-keys or hide-vals"

instance (Gist a, Gist b) => Gist (Either a b) where
  gistPrec' prec conf = parensIfPrecGT 10 prec . \case
    Left  a -> "Left" <+> gistPrec' 11 (fromLast conf confL) a
    Right a -> "Right" <+> gistPrec' 11 (fromLast conf confR) a
   where
    (confL, confR) = configLookups @(Either :&& Either a :& Either a b) conf

instance (Typeable a, Typeable b) => Configurable (Either a b) where
  type ConfigFor (Either a b) = ConfigFor Either
  parseConfigFor = parseConfigFor @Either

instance Typeable a => Configurable (Either a) where
  type ConfigFor (Either a) = ConfigFor Either
  parseConfigFor = parseConfigFor @Either

instance Configurable Either where
  type ConfigFor Either = (Last Config, Last Config)
  parseConfigFor _ = Left "Cannot parse config for Either"

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x

parensIfPrecGT :: Int -> Int -> Doc ann -> Doc ann
parensIfPrecGT comparison prec = if prec > comparison then parens else id
