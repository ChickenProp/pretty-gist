{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Gist.Monadic where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( guard )
import qualified Control.Monad.Trans.Reader    as R
import qualified Data.Dynamic                  as Dyn
import           Data.Functor                   ( (<&>) )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Kind                      ( Type )
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( isJust )
import           Data.Monoid                    ( Last(..) )
import           Data.Proxy                     ( Proxy(..) )
import qualified Prettyprinter                 as PP
import           Prettyprinter                  ( Doc )
import           Type.Reflection                ( SomeTypeRep(..)
                                                , TypeRep
                                                , Typeable
                                                , someTypeRep
                                                , typeRep
                                                )

class (Typeable a, Monoid (ConfigFor a Last), Typeable (ConfigFor a Last))
  => Configurable a
 where
  -- We have to choose between this implementation:
  type ConfigFor a (f :: Type -> Type) :: Type
  type ConfigFor a f = Proxy f
  -- and moving `f` after the equals:
  --
  --     type ConfigFor a :: (Type -> Type) -> Type
  --     type ConfigFor a = Proxy
  --
  -- The latter choice would allow reference to `ConfigFor a` by itself, e.g.
  -- the class constraint could be `Typeable (ConfigFor a)` without reference to
  -- `Last`. But it would also *require* `ConfigFor a` to be referable-to by
  -- itself; e.g. no way to say `ConfigFor SomeType f = f Bool`. If all
  -- instances are custom record types that's not a big problem. But we
  -- plausibly want most instances in future to be [anonymous records][1], and
  -- then it would suck.
  --
  -- [1]: https://hackage.haskell.org/package/large-anon-0.2/docs/Data-Record-Anon-Advanced.html

-- | This concept of a path doesn't give any way to distinguish between e.g. the
-- keys and values of a map. Maybe we instead need a path component to be
-- `(SomeTypeRep, Maybe String)`, with `Nothing` in a matcher matching any
-- `Just` in the path?
newtype GistPath = GistPath { unGistPath :: [NonEmpty SomeTypeRep] }

data PathMatcher
  = PMTail (NonEmpty SomeTypeRep)
  | PMExactPath (NonEmpty SomeTypeRep)
  | PMFuzzy (NonEmpty FuzzyComponent)
  deriving stock (Eq, Ord, Show)

data FuzzyComponent
  = FCMatch SomeTypeRep
  | FCAny01
  | FCAny0N
  | FCAny11
  | FCAny1N
  deriving stock (Eq, Ord, Show)

-- untested
matchPath :: GistPath -> PathMatcher -> Maybe (NonEmpty SomeTypeRep)
matchPath path = \case
  PMTail (t1 :| ts) -> do
    GistPath (p1 : ps) <- pure path
    guard $ t1 `elem` p1
    case NE.nonEmpty ts of
      Just ts' -> NE.cons t1 <$> matchPath (GistPath ps) (PMTail ts')
      Nothing  -> Just $ NE.singleton t1

  PMExactPath (e1 :| es) -> do
    GistPath (p1 : ps) <- pure path
    guard $ e1 `elem` p1
    case NE.nonEmpty es of
      Just es' -> NE.cons e1 <$> matchPath (GistPath ps) (PMExactPath es')
      Nothing  -> do
        guard $ null ps
        Just $ NE.singleton e1

  PMFuzzy fuzzy -> NE.nonEmpty =<< go (unGistPath path) (NE.toList fuzzy)
   where
    go [] []        = Just []
    go [] (f1 : fs) = case f1 of
      FCMatch _ -> Nothing
      FCAny01   -> go [] fs
      FCAny0N   -> go [] fs
      FCAny11   -> Nothing
      FCAny1N   -> Nothing
    go (_  : _ ) []        = Nothing
    go (p1 : ps) (f1 : fs) = case f1 of
      FCMatch t -> do
        guard $ t `elem` p1
        (t :) <$> match1
      FCAny01 -> match0 <|> match1
      FCAny0N -> match0 <|> matchN
      FCAny11 -> match1
      FCAny1N -> match1 <|> matchN
     where
      -- this fuzzy component consumes...
      match0 = go (p1 : ps) fs      -- no path components
      match1 = go ps fs             -- one path component
      matchN = go ps (FCAny0N : fs) -- one or more path components

-- brittany doesn't handle GADT syntax for this.
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
  UnsafeConfig
    { unsafeUnConfig
        :: Map SomeConfigurable (Map (Maybe PathMatcher) Dyn.Dynamic)
    }
  deriving stock Show

instance Semigroup Config where
  UnsafeConfig m1 <> UnsafeConfig m2 = UnsafeConfig $ Map.unionWithKey f m1 m2
   where
    f sc paths1 paths2 = case sc of
      SomeConfigurable c -> Map.unionWith (concatDyns c) paths1 paths2

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

configSingleton
  :: forall a
   . Configurable a
  => ConfigFor a Last
  -> Maybe PathMatcher
  -> Config
configSingleton confFor pm = UnsafeConfig $ Map.singleton
  (SomeConfigurable $ typeRep @a)
  (Map.singleton pm $ Dyn.toDyn confFor)

configLookup
  :: forall a . Configurable a => GistPath -> Config -> ConfigFor a Last
configLookup path (UnsafeConfig m) =
  case Map.lookup (SomeConfigurable $ typeRep @a) m of
    Nothing -> mempty
    Just pathMap ->
      let matchingDyns =
            Map.elems $ flip Map.filterWithKey pathMap $ \k _ ->
              maybe True (isJust . matchPath path) k
      in  mconcat $ matchingDyns <&> \dyn -> case Dyn.fromDynamic dyn of
            Nothing   -> error "Bad Dynamic saved in Config"
            Just conf -> conf

data GistContext = GistContext
  { gcPath :: GistPath
  , gcConf :: Config
  }

class Monad m => MonadGist m where
  gistContext :: m GistContext
  localContext :: GistContext -> m a -> m a

  gistConf :: m Config
  gistConf = gcConf <$> gistContext

  gistPath :: m GistPath
  gistPath = gcPath <$> gistContext

  localPushPath :: NonEmpty (SomeTypeRep) -> m a -> m a
  localPushPath types act = do
    c <- gistContext
    let c' = c { gcPath = GistPath $ types : unGistPath (gcPath c) }
    localContext c' act

newtype GistRunner a = GistRunner { runGistRunner :: GistContext -> a }
  deriving (Functor, Applicative, Monad) via (R.Reader GistContext)

instance MonadGist GistRunner where
  gistContext = GistRunner id
  localContext ctx act = GistRunner $ \_ -> runGistRunner act ctx

-- We use `CL` and `:&` for type-level nonempty lists of Configurables. We can't
-- use regular type-level lists `'[a, b]` because those are hetero-kinded, e.g.
-- `'[ [], [Int] ]` is forbidden.

data CL a

data (:&) a b -- brittany doesn't like `a :& b`.
infixl 5 :&

class ConfigList as where
  type ConfigLookupsResult as (f :: Type -> Type) :: Type
  configLookups :: GistPath -> Config -> ConfigLookupsResult as Last

  toPathComponents :: NonEmpty SomeTypeRep

instance Configurable a => ConfigList (CL a) where
  type ConfigLookupsResult (CL a) f = ConfigFor a f
  configLookups    = configLookup @a

  toPathComponents = someTypeRep (Proxy @a) :| []

instance
  ( ConfigList as
  , Configurable b
  , ConfigFor b Last ~ ConfigLookupsResult as Last
  ) => ConfigList (as :& b) where
  type ConfigLookupsResult (as :& b) f = ConfigFor b f
  configLookups path conf =
    configLookups @as path conf <> configLookup @b path conf

  -- This is O(n^2), but I like having `CL` at the beginning of the list, and n
  -- is small.
  toPathComponents = toPathComponents @as <> toPathComponents @(CL b)

class (ConfigList (GistPathComponents a), Configurable a) => Gist a where
  gistM :: MonadGist m => Int -> a -> m (Doc ann)
  gistM prec val = do
    path <- gistPath
    conf <- configFor @a path <$> gistConf
    localPushPath (toPathComponents @(GistPathComponents a))
      $ renderM prec conf val

  type GistPathComponents a
  renderM :: MonadGist m => Int -> ConfigFor a Identity -> a -> m (Doc ann)
  configFor :: GistPath -> Config -> ConfigFor a Identity

gist :: Gist a => Config -> a -> Doc ann
gist = gistPrec 0

gistPrec :: Gist a => Int -> Config -> a -> Doc ann
gistPrec prec conf val =
  runGistRunner (gistM prec val) $ GistContext (GistPath []) conf

data ConfigForList f = ConfigForList
  { showFirst :: f (Maybe Int)
  }
instance Semigroup (ConfigForList Last) where
  a <> b = ConfigForList (showFirst a <> showFirst b)
instance Monoid (ConfigForList Last) where
  mempty = ConfigForList mempty

instance Configurable [] where
  type ConfigFor [] f = ConfigForList f

instance Typeable a => Configurable [a] where
  type ConfigFor [a] f = ConfigFor [] f

instance Gist a => Gist [a] where
  type GistPathComponents [a] = CL [] :& [a]
  renderM _ (ConfigForList {..}) xs = do
    elems <- case runIdentity showFirst of
      Nothing -> mapM (gistM 0) xs
      Just n  -> case splitAt n xs of
        (start, []   ) -> mapM (gistM 0) start
        (start, _ : _) -> (++ ["..."]) <$> mapM (gistM 0) start
    pure $ PP.align $ PP.list elems
  configFor path conf = ConfigForList (Identity $ fromLast Nothing showFirst)
   where
    ConfigForList {..} = configLookups @(GistPathComponents [a]) path conf

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x
