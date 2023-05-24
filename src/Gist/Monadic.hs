{-# LANGUAGE UndecidableSuperClasses #-}

module Gist.Monadic where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( guard )
import qualified Control.Monad.Trans.Reader    as R
import qualified Data.Dynamic                  as Dyn
import           Data.Functor                   ( (<&>) )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( isJust )
import           Data.Monoid                    ( Last(..) )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Generics                   ( Generic )
import qualified Prettyprinter                 as PP
import           Prettyprinter                  ( Doc )
import qualified Text.Printf                   as Printf
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

config
  :: forall a
   . Configurable a
  => Maybe PathMatcher
  -> ConfigFor a Last
  -> Config
config pm confFor = UnsafeConfig $ Map.singleton
  (SomeConfigurable $ typeRep @a)
  (Map.singleton pm $ Dyn.toDyn confFor)

configF
  :: forall a
   . Configurable a
  => (ConfigFor a Last -> ConfigFor a Last)
  -> Config
configF f = config @a Nothing $ f $ mempty @(ConfigFor a Last)

configPF
  :: forall a
   . Configurable a
  => PathMatcher
  -> (ConfigFor a Last -> ConfigFor a Last)
  -> Config
configPF pm f = config @a (Just pm) $ f $ mempty @(ConfigFor a Last)

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

data CL a

data (:&) a b -- brittany doesn't like `a :& b`.
infixl 5 :&

-- | Type level lists of configurables, `() :& a :& b :& ...`. We can't use
-- type-level lists for this because those are hetero-kinded, e.g. you can't do
-- @'[ '[], '[a] ]@.
--
-- @CL a@ is equivalent to @() :& a@ but nicer to write.
--
-- The types of these functions and the available instances are kinda weird, but
-- it's what I managed to get that works.
class Configurables as where
  type CanDoLookups as x :: Constraint
  configLookups :: CanDoLookups as x => GistPath -> Config -> (x -> x)
  toPathComponents :: NonEmpty SomeTypeRep -> NonEmpty SomeTypeRep

instance Configurables () where
  type CanDoLookups () x = ()
  configLookups _ _ = id
  toPathComponents = id

instance Configurable a => Configurables (CL a) where
  type CanDoLookups (CL a) x = (ConfigFor a Last ~ x)
  configLookups path conf acc = configLookup @a path conf <> acc
  toPathComponents acc = (someTypeRep (Proxy @a) :| []) <> acc

instance
  ( Configurables as
  , Configurable b
  ) => Configurables (as :& b) where
  type CanDoLookups (as :& b) x = (CanDoLookups as x, ConfigFor b Last ~ x)
  configLookups path conf acc =
    configLookups @as path conf (configLookup @b path conf <> acc)

  -- This is O(n^2), but I like having `CL` at the beginning of the list, and n
  -- is small.
  toPathComponents acc = toPathComponents @as $ toPathComponents @(CL b) acc

-- | This needs @UndecidableSuperClasses@. We could put the 'Configurables' and
-- 'CanDoLookups' constraints into the signature of 'gistM' instead, but then we
-- also need them in 'gist', 'gistPrec', the context of the @[a]@ instance...
class
  ( Configurable a
  , Configurables (GistPathComponents a)
  , CanDoLookups (GistPathComponents a) (ConfigFor a Last)
  ) => Gist a
 where
  type GistPathComponents a
  reifyConfig :: ConfigFor a Last -> ConfigFor a Identity
  renderM :: MonadGist m => Int -> ConfigFor a Identity -> a -> m (Doc ann)

gistM :: forall m a ann . (Gist a, MonadGist m) => Int -> a -> m (Doc ann)
gistM prec val = do
  path <- gistPath
  conf <- gistConf
  let thisConf =
        reifyConfig @a
          $ configLookups @(GistPathComponents a) path conf
          $ configLookup @a path conf
  localPushPath
      (toPathComponents @(GistPathComponents a) $ someTypeRep (Proxy @a) :| [])
    $ renderM prec thisConf val

gist :: Gist a => [Config] -> a -> Doc ann
gist = gistPrec 0

gistPrec :: Gist a => Int -> [Config] -> a -> Doc ann
gistPrec prec confs val =
  runGistRunner (gistM prec val) $ GistContext (GistPath []) (mconcat confs)

instance Configurable () where
  type ConfigFor () f = Proxy f

instance Gist () where
  type GistPathComponents () = ()
  reifyConfig _ = Proxy
  renderM _ _ _ = pure "()"

data ConfigFloating f = ConfigFloating
  { printfFmt :: f (Maybe String)
  }
  deriving stock Generic
instance Semigroup (ConfigFloating Last) where
  a <> b = ConfigFloating (printfFmt a <> printfFmt b)
instance Monoid (ConfigFloating Last) where
  mempty = ConfigFloating mempty

instance Configurable Floating where
  type ConfigFor Floating f = ConfigFloating f

instance Configurable Float where
  type ConfigFor Float f = ConfigFor Floating f

instance Gist Float where
  type GistPathComponents Float = CL Floating
  reifyConfig ConfigFloating {..} =
    ConfigFloating (Identity $ fromLast Nothing printfFmt)
  renderM _ (ConfigFloating {..}) f = pure $ case runIdentity printfFmt of
    Nothing  -> PP.viaShow f
    Just fmt -> PP.pretty (Printf.printf fmt f :: String)

instance Configurable Double where
  type ConfigFor Double f = ConfigFor Floating f

instance Gist Double where
  type GistPathComponents Double = CL Floating
  reifyConfig ConfigFloating {..} =
    ConfigFloating (Identity $ fromLast Nothing printfFmt)
  renderM _ (ConfigFloating {..}) f = pure $ case runIdentity printfFmt of
    Nothing  -> PP.viaShow f
    Just fmt -> PP.pretty (Printf.printf fmt f :: String)

-- | Demonstrate that newtype deriving works.
newtype MyFloat = MyFloat Float
  deriving newtype (Floating, Fractional, Num, Show, Configurable, Gist)

data ConfigList f = ConfigList
  { showFirst :: f (Maybe Int)
  }
  deriving stock Generic
instance Semigroup (ConfigList Last) where
  a <> b = ConfigList (showFirst a <> showFirst b)
instance Monoid (ConfigList Last) where
  mempty = ConfigList mempty

instance Configurable [] where
  type ConfigFor [] f = ConfigList f

instance Typeable a => Configurable [a] where
  type ConfigFor [a] f = ConfigFor [] f

instance Gist a => Gist [a] where
  type GistPathComponents [a] = CL []
  reifyConfig ConfigList {..} =
    ConfigList (Identity $ fromLast Nothing showFirst)
  renderM _ (ConfigList {..}) xs = do
    elems <- case runIdentity showFirst of
      Nothing -> mapM (gistM 0) xs
      Just n  -> case splitAt n xs of
        (start, []   ) -> mapM (gistM 0) start
        (start, _ : _) -> (++ ["..."]) <$> mapM (gistM 0) start
    pure $ PP.align $ PP.list elems

instance Configurable (,) where
  type ConfigFor (,) f = Proxy f

instance Typeable a => Configurable ((,) a) where
  type ConfigFor ((,) a) f = ConfigFor (,) f

instance (Typeable a, Typeable b) => Configurable (a, b) where
  type ConfigFor (a, b) f = ConfigFor ((,) a) f

instance (Gist a, Gist b) => Gist (a, b) where
  type GistPathComponents (a, b) = CL (,) :& ((,) a)
  reifyConfig _ = Proxy
  renderM _ _ (a, b) = PP.tupled <$> sequence [gistM 0 a, gistM 0 b]

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x
