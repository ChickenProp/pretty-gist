module Gist
  ( Gist(..)
  , GPMap(..)
  , GPTextQuotes(..)
  ) where

import qualified Data.Char                     as Char
import           Data.Kind                      ( Type )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Last(..) )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Prettyprinter

class Monoid (GistParams a) => Gist a where
  type GistParams a :: Type

  gist' :: GistParams a -> a -> Doc ann
  default gist' :: (GistParams a ~ (), Pretty a) => GistParams a -> a -> Doc ann
  gist' _ = pretty

  gist :: a -> Doc ann
  gist = gist' mempty

instance Gist () where
  type GistParams () = ()

instance Gist Int where
  type GistParams Int = ()

instance Gist Double where
  type GistParams Double = Last (Double -> ShowS)
  gist' = \case
    Last Nothing  -> pretty
    Last (Just f) -> \d -> pretty (f d "")

data GPTextQuotes
  = GPTextQuotesAlways
  | GPTextQuotesNever
  | GPTextQuotesSometimes
  deriving stock (Eq, Show)

instance Gist Text where
  type GistParams Text = Last GPTextQuotes
  gist' q = case fromLast GPTextQuotesSometimes q of
    GPTextQuotesAlways    -> viaShow
    GPTextQuotesNever     -> pretty
    GPTextQuotesSometimes -> \t ->
      if Text.all
           (\c ->
             Char.isAlphaNum c
               || (      Char.generalCategory c
                  `elem` [Char.ConnectorPunctuation, Char.DashPunctuation]
                  )
           )
           t
        then pretty t
        else viaShow t

instance (Gist a, Gist b) => Gist (a, b) where
  type GistParams (a, b) = (GistParams a, GistParams b)
  gist' (pa, pb) (a, b) = tupled [gist' pa a, gist' pb b]

instance Gist a => Gist [a] where
  type GistParams [a] = (Last Int, GistParams a)
  gist' (Last mTake, p) l =
    let elems = case mTake of
          Nothing -> map (gist' p) l
          Just n -> case splitAt n l of
            (start, []) -> map (gist' p) start
            (start, _:_) -> map (gist' p) start ++ ["..."]
    in align $ list elems

data GPMap = GPMap
  { gpMapKeys :: Last Bool
  , gpMapVals :: Last Bool
  }
  deriving stock (Eq, Show)

instance Semigroup GPMap where
  (GPMap a1 b1) <> (GPMap a2 b2) = GPMap (a1 <> a2) (b1 <> b2)
instance Monoid GPMap where
  mempty = GPMap mempty mempty

instance (Gist k, Gist v) => Gist (Map k v) where
  type GistParams (Map k v) = (GPMap, GistParams k, GistParams v)
  gist' (GPMap showKeys showVals, pk, pv) m =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ (showKV <$> Map.toList m)
   where
    showKV (k, v) =
      (if fromLast True showKeys then gist' pk k else "_")
        <> ": "
        <> (if fromLast True showVals then gist' pv v else "_")

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x
