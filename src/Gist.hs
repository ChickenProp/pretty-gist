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
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Kind                      ( Type )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Monoid                    ( Last(..) )
import           Data.Proxy                     ( Proxy(..) )
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
  {-# MINIMAL configFor, (gist' | gistPrec') #-}

  gistPrec' :: Int -> ConfigFor a Identity -> a -> Doc ann
  gistPrec' _ = gist'

  gist' :: ConfigFor a Identity -> a -> Doc ann
  gist' = gistPrec' 0

  configFor :: Config -> ConfigFor a Identity

  gistList' :: ConfigForList a Identity -> [a] -> Doc ann
  default gistList'
    :: (ConfigForList a Identity ~ (Identity (Maybe Int), Identity Config))
    => ConfigForList a Identity
    -> [a]
    -> Doc ann
  gistList' (Identity mTake, Identity subConf) l =
    let elems = case mTake of
          Nothing -> map (gist [subConf]) l
          Just n  -> case splitAt n l of
            (start, []   ) -> map (gist [subConf]) start
            (start, _ : _) -> map (gist [subConf]) start ++ ["..."]
    in align $ list elems

  configForList :: Config -> ConfigForList a Identity
  default configForList
    :: ( ConfigForList a Identity ~ (Identity (Maybe Int), Identity Config)
       , ConfigForList a Last ~ (Last (Maybe Int), Last Config)
       )
    => Config
    -> ConfigForList a Identity
  configForList conf =
    let (lTake, lSubConf) = configLookups @( [] :&& [a] ) conf
    in  (idFromLast Nothing lTake, idFromLast conf lSubConf)

-- | This describes the type of the configuration for a gist.
--
-- Some things are `Configurable` but not `Gist`. In particular, types of kind
-- other than `Type` can be `Configurable`, and gists can look up the config for
-- those types too. So we can have one configuration for `Floating` and a more
-- specific one for `Double`; or one for `[]` and a more specific one for
-- `[()]`.
class
  ( Typeable a
  , Monoid (ConfigFor a Last)
  , Typeable (ConfigFor a Last)
  , Monoid (ConfigForList a Last)
  , Typeable (ConfigForList a Last)
  ) => Configurable a
 where
  type ConfigFor a (f :: Type -> Type) :: Type
  type ConfigFor a f = Proxy f

  parseConfigFor :: String -> Either String (ConfigFor a Last)
  default parseConfigFor
    :: ConfigFor a Last ~ Proxy Last
    => String -> Either String (ConfigFor a Last)
  parseConfigFor _ = Left "Not configurable"

  type ConfigForList a (f :: Type -> Type) :: Type
  type ConfigForList a f = ConfigFor [] f

  parseConfigForList :: String -> Either String (ConfigForList a Last)

  -- If we replace this constrant with `ConfigForList a ~ ConfigFor []`, we get
  -- compile failures for some reason.
  default parseConfigForList
    :: ConfigForList a Last ~ (Last (Maybe Int), Last Config)
    => String
    -> Either String (ConfigForList a Last)
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
          ( Dyn.fromDynamic @(ConfigFor a Last) dyn1
          , Dyn.fromDynamic @(ConfigFor a Last) dyn2
          )
        of
          (Just c1, Just c2) -> Dyn.toDyn $ c1 <> c2
          _                  -> error "Bad Dynamic saved in Config"

instance Monoid Config where
  mempty = UnsafeConfig mempty

instance Gist Config where
  gistPrec' prec (Identity conf) (UnsafeConfig m) = gistPrec prec [conf] m
  configFor conf = idFromLast conf $ configLookup @Config conf

instance Configurable Config where
  type ConfigFor Config f = f Config
  parseConfigFor _ = Left "Cannot parse config for Config"

configInsert
  :: forall a . Configurable a => ConfigFor a Last -> Config -> Config
configInsert confFor (UnsafeConfig m) = UnsafeConfig
  $ Map.insert (SomeConfigurable $ typeRep @a) (Dyn.toDyn confFor) m

configLookup :: forall a . Configurable a => Config -> ConfigFor a Last
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

instance (Configurable a, Configurable b, ConfigFor a Last ~ ConfigFor b Last)
  => ConfigLookups (a :&& b)
 where
  type ConfigLookupsResult (a :&& b) = ConfigFor a Last
  configLookups conf = configLookup @a conf <> configLookup @b conf

instance
  ( Configurable a
  , ConfigLookups as
  , ConfigFor a Last ~ ConfigLookupsResult as
  ) => ConfigLookups (as :& a)
 where
  type ConfigLookupsResult (as :& a) = ConfigFor a Last
  configLookups conf = configLookups @as conf <> configLookup @a conf

-- | For types with a `Pretty` instance, you can derive `Gist` and
-- `Configurable` via `Prettily`. The resulting instances will have no
-- configuration.
newtype Prettily a = Prettily a

instance
  (Configurable a, Pretty a, ConfigFor a Last ~ Proxy Last)
    => Gist (Prettily a)
 where
  gistPrec' _ _ (Prettily a) = pretty a
  configFor _ = Proxy

instance Typeable a => Configurable (Prettily a)

-- | For types with a `Show` instance, you can derive `Gist` and `Configurable`
-- via `Showily`. The resulting instances will have no configuration.
newtype Showily a = Showily a

instance
  (Configurable a, Show a, ConfigFor a Last ~ Proxy Last)
    => Gist (Showily a)
 where
  gistPrec' prec _ (Showily a) = pretty $ showsPrec prec a ""
  configFor _ = Proxy

instance Typeable a => Configurable (Showily a)

-- | Concatenates several configs into one before applying.
--
-- This lets us do `gist [strConfig ..., config ...]`. But maybe we want an
-- `IsConfig` typeclass.
gist :: forall a ann . Gist a => [Config] -> a -> Doc ann
gist confs a = gist' (configFor @a $ mconcat confs) a

-- | Concatenates several configs into one before applying.
--
-- This lets us do `gistPrec n [strConfig ..., config ...]`. But maybe we want
-- an `IsConfig` typeclass.
gistPrec :: forall a ann . Gist a => Int -> [Config] -> a -> Doc ann
gistPrec prec confs a = gistPrec' prec (configFor @a $ mconcat confs) a

-- | Parse a `Config` from a string.
strConfig :: forall a . (HasCallStack, Configurable a) => String -> Config
strConfig s = case parseConfigFor @a s of
  Left  err     -> error $ "Could not parse config: " <> err
  Right confFor -> config @a confFor

-- | Create a singleton `Config`.
config :: forall a . Configurable a => ConfigFor a Last -> Config
config confFor = configInsert @a confFor mempty

instance Gist a => Gist [a] where
  gistPrec' _ = gistList' @a
  configFor conf = configForList @a conf

instance Configurable a => Configurable [a] where
  type ConfigFor [a] f = ConfigForList a f
  parseConfigFor = parseConfigForList @a

-- | TODO: no way in strConfig to reset to default behavior.
instance Configurable [] where
  type ConfigFor [] f = (f (Maybe Int), f Config)
  parseConfigFor s = case words s of
    ["show-first", n] -> case readMaybe n of
      Just n' -> Right (pure (Just n'), mempty)
      Nothing -> Left "Expected \"show-first (int)\""
    _ -> Left "Expected \"show-first (int)\""

instance Gist a => Gist (Maybe a) where
  gistPrec' prec (Identity showConstructors, Identity subConf) val =
    if showConstructors
      then case val of
        Nothing -> "Nothing"
        Just v  -> parensIfPrecGT 10 prec $ "Just" <+> gistPrec 11 [subConf] v
      else case val of
        Nothing -> "_"
        Just v  -> gistPrec prec [subConf] v
  configFor conf =
    let (lShowConstructors, lSubConf) = configLookups @(Maybe :&& Maybe a) conf
    in  (idFromLast False lShowConstructors, idFromLast conf lSubConf)

instance Typeable a => Configurable (Maybe a) where
  type ConfigFor (Maybe a) f = ConfigFor Maybe f
  parseConfigFor = parseConfigFor @Maybe

instance Configurable Maybe where
  type ConfigFor Maybe f = (f Bool, f Config)
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
  gistPrec' _ (Identity fmt) = case fmt of
    Nothing -> pretty
    Just f  -> \d -> pretty $ Printf.formatRealFloat d f ""
  configFor conf =
    idFromLast Nothing $ configLookups @(Floating :&& Float) conf

instance Configurable Float where
  type ConfigFor Float f = ConfigFor Floating f
  parseConfigFor = parseConfigFor @Floating

instance Gist Double where
  gistPrec' _ (Identity fmt) = case fmt of
    Nothing -> pretty
    Just f  -> \d -> pretty $ Printf.formatRealFloat d f ""
  configFor conf =
    idFromLast Nothing $ configLookups @(Floating :&& Double) conf

instance Configurable Double where
  type ConfigFor Double f = ConfigFor Floating f
  parseConfigFor = parseConfigFor @Floating

-- | TODO: allow comma and underscore separation. Also, there's no way for
-- strConfig to revert to the default behavior. And there's no instance for
-- `Show FieldFormat`.
instance Configurable Floating where
  type ConfigFor Floating f = f (Maybe Printf.FieldFormat)
  parseConfigFor str = pure . Just <$> go str
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
  type ConfigFor IsString f = f ConfStrQuotes
  parseConfigFor = \case
    "quotes-always"    -> Right $ pure ConfStrQuotesAlways
    "quotes-never"     -> Right $ pure ConfStrQuotesNever
    "quotes-sometimes" -> Right $ pure ConfStrQuotesSometimes
    _                  -> Left "unknown quote specifier"

instance Gist Char where
  gistPrec' _ (Identity conf) c = case conf of
    ConfStrQuotesAlways    -> viaShow c
    ConfStrQuotesNever     -> pretty c
    ConfStrQuotesSometimes -> if charWantsQuotes c then viaShow c else pretty c
  configFor =
    idFromLast ConfStrQuotesSometimes . configLookups @(IsString :&& Char)

  gistList' (Identity conf) s = case conf of
    ConfStrQuotesAlways -> viaShow s
    ConfStrQuotesNever  -> pretty s
    ConfStrQuotesSometimes ->
      if any charWantsQuotes s then viaShow s else pretty s
  configForList =
    idFromLast ConfStrQuotesSometimes . configLookups @(IsString :&& String)

instance Configurable Char where
  type ConfigFor Char f = ConfigFor IsString f
  parseConfigFor = parseConfigFor @IsString

  type ConfigForList Char f = f ConfStrQuotes
  parseConfigForList = parseConfigFor @Char

instance Gist Text where
  gistPrec' _ conf s = gist' conf (Text.unpack s)
  configFor =
    idFromLast ConfStrQuotesSometimes . configLookups @(IsString :&& Text)

instance Configurable Text where
  type ConfigFor Text f = ConfigFor String f
  parseConfigFor = parseConfigFor @String

instance (Gist a, Gist b) => Gist (a, b) where
  gistPrec' _ (Identity confA, Identity confB) (a, b) =
    tupled [gist [confA] a, gist [confB] b]
  configFor conf = (idFromLast conf confA, idFromLast conf confB)
    where (confA, confB) = configLookups @((,) :&& ((,) a) :& (a, b)) conf

instance (Typeable a, Typeable b) => Configurable (a, b) where
  type ConfigFor (a, b) f = (f Config, f Config)
  parseConfigFor _ = Left "Cannot parse config for (a, b)"

instance Configurable (,) where
  type ConfigFor (,) f = (f Config, f Config)
  parseConfigFor _ = Left "Cannot parse config for (,)"

instance Typeable a => Configurable ((,) a) where
  type ConfigFor ((,) a) f = (f Config, f Config)
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
  gistPrec' _ conf m =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ (showKV <$> Map.toList m)
   where
    (ConfMap showKeys showVals, (Identity confK), (Identity confV)) = conf
    showKV (k, v) =
      (if runIdentity showKeys then gist [confK] k else "_")
        <> ": "
        <> (if runIdentity showVals then gist [confV] v else "_")

  configFor conf =
    ( ConfMap (idFromLast True showKeys) (idFromLast True showVals)
    , idFromLast conf confK
    , idFromLast conf confV
    )
   where
    (ConfMap showKeys showVals, confK, confV) =
      configLookups @(Map :&& Map k :& Map k v) conf

instance (Typeable k, Typeable v) => Configurable (Map k v) where
  type ConfigFor (Map k v) f = ConfigFor Map f
  parseConfigFor = parseConfigFor @Map

instance Typeable k => Configurable (Map k) where
  type ConfigFor (Map k) f = ConfigFor Map f
  parseConfigFor = parseConfigFor @Map

instance Configurable Map where
  type ConfigFor Map f = (ConfMap f, f Config, f Config)
  parseConfigFor = \case
    "hide-keys" -> Right (ConfMap (pure False) mempty, mempty, mempty)
    "hide-vals" -> Right (ConfMap mempty (pure False), mempty, mempty)
    _           -> Left "Expected hide-keys or hide-vals"

instance (Gist a, Gist b) => Gist (Either a b) where
  gistPrec' prec (Identity confL, Identity confR) =
    parensIfPrecGT 10 prec . \case
      Left  a -> "Left" <+> gistPrec 11 [confL] a
      Right a -> "Right" <+> gistPrec 11 [confR] a
  configFor conf = (idFromLast conf confL, idFromLast conf confR)
   where
    (confL, confR) = configLookups @(Either :&& Either a :& Either a b) conf

instance (Typeable a, Typeable b) => Configurable (Either a b) where
  type ConfigFor (Either a b) f = ConfigFor Either f
  parseConfigFor = parseConfigFor @Either

instance Typeable a => Configurable (Either a) where
  type ConfigFor (Either a) f = ConfigFor Either f
  parseConfigFor = parseConfigFor @Either

instance Configurable Either where
  type ConfigFor Either f = (f Config, f Config)
  parseConfigFor _ = Left "Cannot parse config for Either"

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x

idFromLast :: a -> Last a -> Identity a
idFromLast def = Identity . fromLast def

parensIfPrecGT :: Int -> Int -> Doc ann -> Doc ann
parensIfPrecGT comparison prec = if prec > comparison then parens else id
