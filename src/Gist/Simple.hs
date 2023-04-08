{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Gist.Simple
  ( Gist(..)
  ) where

import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
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

  gist = gistPrec 0
  gistPrec _ = gist

-- brittany doesn't handle GADT syntax for this.
data Gister a
  = FnGister (forall ann . a -> Doc ann)
  | Gist a => ConfGister (Config a)

runGister :: Gister a -> a -> Doc ann
runGister = \case
  FnGister   f -> f
  ConfGister c -> gist c

data ConfigFloating = ConfigFloating
  { printfFmt :: Maybe String
  }

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

data ConfigList a = ConfigList
  { gistElem  :: Gister a
  , showFirst :: Maybe Int
  }

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

instance Gist (Set a) where
  type Config (Set a) = ConfigSet a
  type HasDefaultConfig (Set a) = (Gist a, HasDefaultConfig a)
  defaultConfig = ConfigSet (ConfGister $ defaultConfig @a)

  gistPrec _ (ConfigSet {..}) xs =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ (runGister gistElem <$> Set.toList xs)
