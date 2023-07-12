module Gist.Simple
  ( Gist(..)
  , Gister(..)
  , Showily(..)
  , ConfigPrintf(..)
  , ConfigMaybe(..)
  , ConfigList(..)
  , ConfigSet(..)
  , MyFloat(..)
  , defaultConfGister
  , defaultConfGisterF
  , runGister
  , record
  ) where

import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import           Data.Maybe                     ( isJust )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           GHC.Generics                   ( Generic )
import           Prettyprinter
import qualified Text.Printf                   as Printf

newtype Prec = Prec Int
  deriving newtype (Eq, Ord, Num)

class Gist a where
  {-# MINIMAL gist | gistPrec #-}

  type Config a :: Type
  type Config a = ()

  type HasDefaultConfig a :: Constraint
  type HasDefaultConfig a = ()

  defaultConfig :: HasDefaultConfig a => Config a
  default defaultConfig :: (Config a ~ ()) => Config a
  defaultConfig = ()

  gistPrec :: Prec -> Config a -> a -> Doc ann
  gist :: Config a -> a -> Doc ann

  gistPrecF
    :: HasDefaultConfig a => Prec -> (Config a -> Config a) -> a -> Doc ann
  gistF :: HasDefaultConfig a => (Config a -> Config a) -> a -> Doc ann
  gistPrec_ :: HasDefaultConfig a => Prec -> a -> Doc ann
  gist_ :: HasDefaultConfig a => a -> Doc ann

  gist = gistPrec 0
  gistPrec _ = gist
  gistF f = gist (f $ defaultConfig @a)
  gistPrecF p f = gistPrec p (f $ defaultConfig @a)
  gist_ = gist (defaultConfig @a)
  gistPrec_ prec = gistPrec prec (defaultConfig @a)

-- brittany doesn't handle GADT syntax for this.
-- data Gister a where
--   FnGister :: (forall ann . Int -> a -> Doc ann) -> Gister a
--   ConfGister :: Gist a => Config a -> Gister a
data Gister a
  = FnGister (forall ann . Prec -> a -> Doc ann)
  | Gist a => ConfGister (Config a)
-- cannot derive generic

defaultConfGister :: forall a . (Gist a, HasDefaultConfig a) => Gister a
defaultConfGister = ConfGister $ defaultConfig @a

defaultConfGisterF
  :: forall a
   . (Gist a, HasDefaultConfig a)
  => (Config a -> Config a)
  -> Gister a
defaultConfGisterF f = ConfGister $ f $ defaultConfig @a

runGisterPrec :: Prec -> Gister a -> a -> Doc ann
runGisterPrec prec = \case
  FnGister   f -> f prec
  ConfGister c -> gistPrec prec c

runGister :: Gister a -> a -> Doc ann
runGister = runGisterPrec 0

newtype Showily a = Showily a
instance Show a => Gist (Showily a) where
  type Config (Showily a) = ()
  type HasDefaultConfig (Showily a) = ()
  gistPrec (Prec prec) _ (Showily a) = pretty $ showsPrec prec a ""

instance Gist Void where
  gist _ = absurd

instance Gist () where
  gist _ _ = "()"

data ConfigMaybe a = ConfigMaybe
  { showConstructors :: Bool
  , gistElem         :: Gister a
  }
  deriving stock Generic

instance Gist (Maybe a) where
  type Config (Maybe a) = ConfigMaybe a
  type HasDefaultConfig (Maybe a) = (Gist a, HasDefaultConfig a)
  defaultConfig = ConfigMaybe False (ConfGister $ defaultConfig @a)

  gistPrec prec (ConfigMaybe {..}) = if showConstructors
    then \case
      Nothing -> "Nothing"
      Just x  -> parensIf (prec > 10) $ "Just" <+> runGisterPrec 11 gistElem x
    else \case
      Nothing -> "_"
      Just x  -> runGisterPrec prec gistElem x

data ConfigPrintf = ConfigPrintf
  { printfFmt :: Maybe String
  }
  deriving stock Generic

newtype Printfily a = Printfily a
instance (Show a, Printf.PrintfArg a) => Gist (Printfily a) where
  type Config (Printfily a) = ConfigPrintf
  defaultConfig = ConfigPrintf Nothing

  gist (ConfigPrintf {..}) (Printfily a) = case printfFmt of
    Nothing  -> viaShow a
    Just fmt -> pretty (Printf.printf fmt a :: String)

deriving via Printfily Int instance Gist Int
deriving via Printfily Float instance Gist Float
deriving via Printfily Double instance Gist Double

-- | Demonstrate that newtype deriving works.
newtype MyFloat = MyFloat Float
  deriving newtype (Floating, Fractional, Num, Show, Gist)

data ConfigList a = ConfigList
  { gistElem  :: Gister a
  , showFirst :: Maybe Int
  }
  deriving stock Generic

instance Gist [a] where
  type Config [a] = ConfigList a
  type HasDefaultConfig [a] = (Gist a, HasDefaultConfig a)
  defaultConfig = ConfigList (ConfGister $ defaultConfig @a) Nothing

  gistPrec _ (ConfigList {..}) xs =
    let elems = case showFirst of
          Nothing -> runGister gistElem <$> xs
          Just n  -> case splitAt n xs of
            (start, []   ) -> runGister gistElem <$> start
            (start, _ : _) -> (runGister gistElem <$> start) ++ ["..."]
    in  align $ list elems

data ConfigSet a = ConfigSet
  { gistElem :: Gister a
  }
  deriving stock Generic

instance Gist (Set a) where
  type Config (Set a) = ConfigSet a
  type HasDefaultConfig (Set a) = (Gist a, HasDefaultConfig a)
  defaultConfig = ConfigSet (ConfGister $ defaultConfig @a)

  gistPrec _ (ConfigSet {..}) xs =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ (runGister gistElem <$> Set.toList xs)

data ConfigTuple2 a b = ConfigTuple2
  { gistFst :: Gister a
  , gistSnd :: Gister b
  }
  deriving stock Generic

instance Gist (a, b) where
  type Config (a, b) = ConfigTuple2 a b
  type HasDefaultConfig (a, b)
    = (Gist a, Gist b, HasDefaultConfig a, HasDefaultConfig b)
  defaultConfig = ConfigTuple2 (ConfGister $ defaultConfig @a)
                               (ConfGister $ defaultConfig @b)

  gistPrec _ (ConfigTuple2 {..}) (a, b) =
    tupled [runGister gistFst a, runGister gistSnd b]

parensIf :: Bool -> Doc ann -> Doc ann
parensIf cond = if cond then parens else id

record :: Prec -> Maybe (Doc ann) -> [(Doc ann, Doc ann)] -> Doc ann
record prec mConstr fields =
  parensIf (prec > 10 && isJust mConstr)
    $ maybe id (\constr contents -> constr <+> align contents) mConstr
    $ group
    $ encloseSep (flatAlt "{ " "{") (flatAlt "\n}" "}") ", "
    $ flip map fields
    $ \(key, val) -> key <+> "=" <+> val
