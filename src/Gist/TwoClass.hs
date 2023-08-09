{-# LANGUAGE UndecidableSuperClasses #-}

module Gist.TwoClass where

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
import           Data.Traversable               ( for )
import           GHC.Generics                   ( Generic )
import qualified Prettyprinter                 as PP
import           Prettyprinter                  ( (<+>)
                                                , Doc
                                                )
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
  -- would `ConfigFor (f :: Type -> Type) a :: Type` work? Would it be nicer?

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
newtype GistPath =
  GistPath { unGistPath :: [(NonEmpty SomeTypeRep, Maybe String)] }

data PathMatcher
  = PMTail (NonEmpty (SomeTypeRep, [String]))
  | PMExactPath (NonEmpty (SomeTypeRep, [String]))
  | PMFuzzy (NonEmpty FuzzyComponent)
  deriving stock (Eq, Ord, Show)

data FuzzyComponent
  = FCMatch (SomeTypeRep, [String])
  | FCAny01
  | FCAny0N
  | FCAny11
  | FCAny1N
  deriving stock (Eq, Ord, Show)

-- untested
matchPath
  :: GistPath -> PathMatcher -> Maybe (NonEmpty (SomeTypeRep, Maybe String))
matchPath path = \case
  PMTail x -> matchPath path (PMFuzzy $ FCAny0N `NE.cons` (FCMatch <$> x))
  PMExactPath x -> matchPath path (PMFuzzy $ FCMatch <$> x)

  PMFuzzy fuzzy -> NE.nonEmpty
    =<< go (unGistPath path) (reverse $ NE.toList fuzzy)
   where
    go [] []        = Just []
    go [] (f1 : fs) = case f1 of
      FCMatch _ -> Nothing
      FCAny01   -> go [] fs
      FCAny0N   -> go [] fs
      FCAny11   -> Nothing
      FCAny1N   -> Nothing
    go (_        : _ ) []        = Nothing
    go ((p1, mc) : ps) (f1 : fs) = case f1 of
      FCMatch (t, ss) -> do
        guard $ t `elem` p1
        guard $ null ss || maybe False (`elem` ss) mc
        ((t, mc) :) <$> match1
      FCAny01 -> match0 <|> match1
      FCAny0N -> match0 <|> matchN
      FCAny11 -> match1
      FCAny1N -> match1 <|> matchN
     where
      -- this fuzzy component consumes...
      match0 = go ((p1, mc) : ps) fs -- no path components
      match1 = go ps fs              -- one path component
      matchN = go ps (FCAny0N : fs)  -- one or more path components

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

  -- | This pushes the config on the left, so that it's less specific than the
  -- user's choices, and also less specific than higher-level calls to this
  -- function. So
  --
  --     localPushConf c1 $ localPushConf c2 $ ...
  --
  -- has `c1` override `c2`, which is probably good.
  localPushConf :: Config -> m a -> m a
  localPushConf conf act = do
    c <- gistContext
    let c' = c { gcConf = conf <> gcConf c }
    localContext c' act

  gistPath :: m GistPath
  gistPath = gcPath <$> gistContext

  localPushPath :: NonEmpty (SomeTypeRep) -> Maybe String -> m a -> m a
  localPushPath types mComponent act = do
    c <- gistContext
    let c' = c
          { gcPath = GistPath $ (types, Nothing) : case unGistPath (gcPath c) of
                       []            -> []
                       (ts1, _) : ts -> (ts1, mComponent) : ts
          }
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
-- 'CanDoLookups' constraints into the signature of 'subGist' instead, but then
-- we also need them in 'gist', 'gistPrec', the context of the @[a]@ instance...
class
  ( Configurable a
  , Configurables (GistLookups a)
  , CanDoLookups (GistLookups a) (ConfigFor a Last)
  ) => Gist a
 where
  type GistLookups a
  reifyConfig :: ConfigFor a Last -> ConfigFor a Identity
  renderM :: MonadGist m => Int -> ConfigFor a Identity -> a -> m (Doc ann)

subGistPrec
  :: forall m a ann
   . (Gist a, MonadGist m)
  => Int
  -> Maybe String
  -> a
  -> m (Doc ann)
subGistPrec prec mComponent val = do
  path <- gistPath
  conf <- gistConf
  let thisConf =
        reifyConfig @a
          $ configLookups @(GistLookups a) path conf
          $ configLookup @a path conf
  localPushPath
      (toPathComponents @(GistLookups a) $ someTypeRep (Proxy @a) :| [])
      mComponent
    $ renderM prec thisConf val

subGist
  :: forall m a ann . (Gist a, MonadGist m) => Maybe String -> a -> m (Doc ann)
subGist = subGistPrec 0

gist :: Gist a => [Config] -> a -> Doc ann
gist = gistPrec 0

gistPrec :: Gist a => Int -> [Config] -> a -> Doc ann
gistPrec prec confs val =
  runGistRunner (subGistPrec prec Nothing val)
    $ GistContext (GistPath []) (mconcat confs)

newtype Showily a = Showily a
instance Typeable a => Configurable (Showily a) where
  type ConfigFor (Showily a) f = Proxy f
instance (Show a, Typeable a) => Gist (Showily a) where
  type GistLookups (Showily a) = ()
  reifyConfig _ = Proxy
  renderM prec _ (Showily a) = pure $ PP.pretty $ showsPrec prec a ""

instance Configurable () where
  type ConfigFor () f = Proxy f

instance Gist () where
  type GistLookups () = ()
  reifyConfig _ = Proxy
  renderM _ _ _ = pure "()"

data ConfigPrintf f = ConfigPrintf
  { printfFmt :: f (Maybe String)
  }
  deriving stock Generic
instance Semigroup (ConfigPrintf Last) where
  a <> b = ConfigPrintf (printfFmt a <> printfFmt b)
instance Monoid (ConfigPrintf Last) where
  mempty = ConfigPrintf mempty

reifyConfigPrintf :: ConfigPrintf Last -> ConfigPrintf Identity
reifyConfigPrintf (ConfigPrintf {..}) =
  ConfigPrintf (Identity $ fromLast Nothing printfFmt)

renderPrintf
  :: (Show a, Printf.PrintfArg a) => ConfigPrintf Identity -> a -> Doc ann
renderPrintf (ConfigPrintf {..}) x = case runIdentity printfFmt of
  Nothing  -> PP.viaShow x
  Just fmt -> PP.pretty (Printf.printf fmt x :: String)

instance Configurable Int where
  type ConfigFor Int f = ConfigPrintf f

instance Gist Int where
  type GistLookups Int = ()
  reifyConfig = reifyConfigPrintf
  renderM _ conf x = pure $ renderPrintf conf x

instance Configurable Floating where
  type ConfigFor Floating f = ConfigPrintf f

instance Configurable Float where
  type ConfigFor Float f = ConfigFor Floating f

instance Gist Float where
  type GistLookups Float = CL Floating
  reifyConfig = reifyConfigPrintf
  renderM _ conf x = pure $ renderPrintf conf x

instance Configurable Double where
  type ConfigFor Double f = ConfigFor Floating f

instance Gist Double where
  type GistLookups Double = CL Floating
  reifyConfig = reifyConfigPrintf
  renderM _ conf x = pure $ renderPrintf conf x

-- | Demonstrate that newtype deriving works.
newtype MyFloat = MyFloat Float
  deriving newtype (Floating, Fractional, Num, Show, Configurable, Gist)

data ConfigMaybe f = ConfigMaybe
  { showConstructors :: f Bool
  }
  deriving stock Generic
instance Semigroup (ConfigMaybe Last) where
  a <> b = ConfigMaybe (showConstructors a <> showConstructors b)
instance Monoid (ConfigMaybe Last) where
  mempty = ConfigMaybe mempty

instance Configurable Maybe where
  type ConfigFor Maybe f = ConfigMaybe f
instance Typeable a => Configurable (Maybe a) where
  type ConfigFor (Maybe a) f = ConfigFor Maybe f

instance Gist a => Gist (Maybe a) where
  type GistLookups (Maybe a) = CL Maybe
  reifyConfig (ConfigMaybe {..}) =
    ConfigMaybe (Identity $ fromLast False showConstructors)
  renderM prec (ConfigMaybe {..}) = if runIdentity showConstructors
    then \case
      Nothing -> pure "Nothing"
      Just x  -> do
        renderedElem <- subGistPrec 11 Nothing x
        pure $ parensIfPrecGT 10 prec $ "Just" <+> renderedElem
    else \case
      Nothing -> pure "_"
      Just x  -> subGistPrec prec Nothing x

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
  type GistLookups [a] = CL []
  reifyConfig ConfigList {..} =
    ConfigList (Identity $ fromLast Nothing showFirst)
  renderM _ (ConfigList {..}) xs = do
    elems <- case runIdentity showFirst of
      Nothing -> mapM (subGist Nothing) xs
      Just n  -> case splitAt n xs of
        (start, []   ) -> mapM (subGist Nothing) start
        (start, _ : _) -> (++ ["..."]) <$> mapM (subGist Nothing) start
    pure $ PP.align $ PP.list elems

instance Configurable (,) where
  type ConfigFor (,) f = Proxy f

instance Typeable a => Configurable ((,) a) where
  type ConfigFor ((,) a) f = ConfigFor (,) f

instance (Typeable a, Typeable b) => Configurable (a, b) where
  type ConfigFor (a, b) f = ConfigFor ((,) a) f

instance (Gist a, Gist b) => Gist (a, b) where
  type GistLookups (a, b) = CL (,) :& ((,) a)
  reifyConfig _ = Proxy
  renderM _ _ (a, b) =
    PP.tupled <$> sequence [subGist (Just "fst") a, subGist (Just "snd") b]

parensIf :: Bool -> Doc ann -> Doc ann
parensIf cond = if cond then PP.parens else id

parensIfPrecGT :: Int -> Int -> Doc ann -> Doc ann
parensIfPrecGT comparison prec = parensIf $ prec > comparison

record
  :: MonadGist m
  => Int
  -> Maybe (Doc ann)
  -> [(Doc ann, m (Doc ann))]
  -> m (Doc ann)
record prec mConstr fields = do
  renderedFields <- for fields
    $ \(key, val) -> (\v -> key <+> "=" <+> v) <$> val
  pure
    $ parensIf (prec > 10 && isJust mConstr)
    $ maybe id (\constr contents -> constr <+> PP.align contents) mConstr
    $ PP.group
    $ PP.encloseSep (PP.flatAlt "{ " "{") (PP.flatAlt "\n}" "}") ", "
    $ renderedFields

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x
