module Gist.Simple
  ( Gist(..)
  , Gister(..)
  , ConfigFloating(..)
  , MyFloat(..)
  ) where

import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           GHC.Generics                   ( Generic )
import           Prettyprinter
import qualified Text.Printf                   as Printf

class Gist a where
  {-# MINIMAL gist | gistPrec #-}

  type Config a :: Type
  type Config a = ()

  type HasDefaultConfig a :: Constraint
  type HasDefaultConfig a = ()

  defaultConfig :: HasDefaultConfig a => Config a
  default defaultConfig :: (Config a ~ ()) => Config a
  defaultConfig = ()

  gistPrec :: Int -> Config a -> a -> Doc ann
  gist :: Config a -> a -> Doc ann

  gistPrecF
    :: HasDefaultConfig a => Int -> (Config a -> Config a) -> a -> Doc ann
  gistF :: HasDefaultConfig a => (Config a -> Config a) -> a -> Doc ann

  gist = gistPrec 0
  gistPrec _ = gist
  gistF f = gist (f $ defaultConfig @a)
  gistPrecF p f = gistPrec p (f $ defaultConfig @a)

-- brittany doesn't handle GADT syntax for this.
-- data Gister a where
--   FnGister :: (forall ann . Int -> a -> Doc ann) -> Gister a
--   ConfGister :: Gist a => Config a -> Gister a
data Gister a
  = FnGister (forall ann . Int -> a -> Doc ann)
  | Gist a => ConfGister (Config a)
-- cannot derive generic

runGisterPrec :: Int -> Gister a -> a -> Doc ann
runGisterPrec prec = \case
  FnGister   f -> f prec
  ConfGister c -> gistPrec prec c

runGister :: Gister a -> a -> Doc ann
runGister = runGisterPrec 0

instance Gist Void where
  gist _ = absurd

instance Gist () where
  gist _ _ = "()"

data ConfigFloating = ConfigFloating
  { printfFmt :: Maybe String
  }
  deriving stock Generic

instance Gist Float where
  type Config Float = ConfigFloating
  defaultConfig = ConfigFloating Nothing

  gistPrec _ conf f = case printfFmt conf of
    Nothing  -> viaShow f
    Just fmt -> pretty (Printf.printf fmt f :: String)

instance Gist Double where
  type Config Double = Config Float
  defaultConfig = defaultConfig @Float

  gistPrec _ conf d = case printfFmt conf of
    Nothing  -> viaShow d
    Just fmt -> pretty (Printf.printf fmt d :: String)

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
