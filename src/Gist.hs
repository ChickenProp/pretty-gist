module Gist
  ( Gist(..)
  , Configurable(..)
  , Config(..)
  , config
  , strConfig
  , gist
  , ConfMap(..)
  , ConfStrQuotes(..)
  ) where

import           Data.Bifunctor                 ( first )
import qualified Data.Char                     as Char
import qualified Data.Dynamic                  as Dyn
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
class Configurable a => Gist a where
  gist' :: Config -> a -> Doc ann
  default gist'
    :: (ConfigFor a ~ (), Pretty a) => Config -> a -> Doc ann
  gist' conf = pretty
   -- This clause avoids a "redundant constraint" warning, but shouldn't affect
   -- semantics (including laziness):
   where _x :: () = configLookup @a conf

  gistList' :: Config -> [a] -> Doc ann
  default gistList'
    :: (ConfigForList a ~ (Last Int, Last Config))
    => Config
    -> [a]
    -> Doc ann
  gistList' conf l =
    let (Last mTake, Last mSubConf) =
          configLookups @( [] :&& [a] ) conf
        subConf = fromMaybe conf mSubConf
        elems = case mTake of
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
    :: ConfigForList a ~ (Last Int, Last Config)
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
          ( Dyn.fromDynamic @(ConfigFor a) dyn1
          , Dyn.fromDynamic @(ConfigFor a) dyn2
          )
        of
          (Just c1, Just c2) -> Dyn.toDyn $ c1 <> c2
          _                  -> error "Bad Dynamic saved in Config"

instance Monoid Config where
  mempty = UnsafeConfig mempty

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
  gist' = gistList' @a

instance Configurable a => Configurable [a] where
  type ConfigFor [a] = ConfigForList a
  parseConfigFor = parseConfigForList @a

instance Configurable [] where
  type ConfigFor [] = (Last Int, Last Config)
  parseConfigFor s = case words s of
    ["show-first", n] -> case readMaybe n of
      Just n' -> Right (pure n', mempty)
      Nothing -> Left "Expected \"show-first (int)\""
    _ -> Left "Expected \"show-first (int)\""

instance Gist ()
instance Configurable ()

instance Gist Int

-- | TODO: allow comma and underscore separation.
instance Configurable Int

instance Gist Float where
  gist' conf = case configLookups @(Floating :&& Float) conf of
    Last Nothing  -> pretty
    Last (Just f) -> \d -> pretty $ Printf.formatRealFloat d f ""

instance Configurable Float where
  type ConfigFor Float = ConfigFor Floating
  parseConfigFor = parseConfigFor @Floating

instance Gist Double where
  gist' conf = case configLookups @(Floating :&& Double) conf of
    Last Nothing  -> pretty
    Last (Just f) -> \d -> pretty $ Printf.formatRealFloat d f ""

instance Configurable Double where
  type ConfigFor Double = ConfigFor Floating
  parseConfigFor = parseConfigFor @Floating

-- | TODO: allow comma and underscore separation.
instance Configurable Floating where
  type ConfigFor Floating = Last Printf.FieldFormat
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
  type ConfigFor IsString = Last ConfStrQuotes
  parseConfigFor = \case
    "quotes-always"    -> Right $ pure ConfStrQuotesAlways
    "quotes-never"     -> Right $ pure ConfStrQuotesNever
    "quotes-sometimes" -> Right $ pure ConfStrQuotesSometimes
    _                  -> Left "unknown quote specifier"

instance Gist Char where
  gist' conf c =
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
  gist' conf s =
    let myConf = configLookups @(IsString :&& Text) conf
    in  gist' (configInsert @String myConf conf) (Text.unpack s)

instance Configurable Text where
  type ConfigFor Text = ConfigFor String
  parseConfigFor = parseConfigFor @String

instance (Gist a, Gist b) => Gist (a, b) where
  gist' conf (a, b) = tupled
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
  gist' conf m =
    let
      (ConfMap showKeys showVals, confK, confV) =
        configLookups @(Map :&& Map k :& Map k v) conf
      showKV (k, v) =
        (if fromLast True showKeys then gist' (fromLast conf confK) k else "_")
          <> ": "
          <> (if fromLast True showVals
               then gist' (fromLast conf confV) v
               else "_"
             )
    in
      group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ (showKV <$> Map.toList m)

instance (Typeable k, Typeable v) => Configurable (Map k v) where
  type ConfigFor (Map k v) = ConfigFor Map
  parseConfigFor = parseConfigFor @Map

instance (Typeable k) => Configurable (Map k) where
  type ConfigFor (Map k) = ConfigFor Map
  parseConfigFor = parseConfigFor @Map

instance Configurable Map where
  type ConfigFor Map = (ConfMap, Last Config, Last Config)
  parseConfigFor = \case
    "hide-keys" -> Right (ConfMap (pure False) mempty, mempty, mempty)
    "hide-vals" -> Right (ConfMap mempty (pure False), mempty, mempty)
    _           -> Left "Expected hide-keys or hide-vals"

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x
