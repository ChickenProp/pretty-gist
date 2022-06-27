module Gist
  ( Gist(..)
  , GPMap(..)
  , GPStrQuotes(..)
  ) where

import           Data.Bifunctor                 ( first )
import qualified Data.Char                     as Char
import           Data.Functor                   ( (<&>) )
import           Data.Kind                      ( Type )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Last(..) )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Prettyprinter
import qualified Text.Printf                   as Printf
import           Text.Read                      ( readMaybe )

class (Monoid (GistParams a), Monoid (GistParamsList a)) => Gist a where
  type GistParams a :: Type

  gist' :: GistParams a -> a -> Doc ann
  default gist' :: (GistParams a ~ (), Pretty a) => GistParams a -> a -> Doc ann
  gist' _ = pretty

  gist :: a -> Doc ann
  gist = gist' mempty

  gistP :: [String] -> a -> Doc ann
  gistP ps = gist' (mconcat $ map (parseParam @a) ps)

  parseParam :: String -> GistParams a
  parseParam _ = mempty

  -- | The instance for `Gist [a]` delegates to this. Customize it if you want
  -- to customize that instance. Needed for `Gist String`.
  type GistParamsList a :: Type
  type GistParamsList a = (Last Int, GistParams a)

  -- | The instance for `Gist [a]` delegates to this. Customize it if you want
  -- to customize that instance. Needed for `Gist String`.
  gistList' :: GistParamsList a -> [a] -> Doc ann
  default gistList'
    :: GistParamsList a ~ (Last Int, GistParams a)
    => GistParamsList a -> [a] -> Doc ann
  gistList' (Last mTake, p) l =
    let elems = case mTake of
          Nothing -> map (gist' p) l
          Just n  -> case splitAt n l of
            (start, []   ) -> map (gist' p) start
            (start, _ : _) -> map (gist' p) start ++ ["..."]
    in  align $ list elems

  -- | The instance for `Gist [a]` delegates to this. Customize it if you want
  -- to customize that instance. Needed for `Gist String`.
  parseParamList :: String -> GistParamsList a
  default parseParamList
    :: GistParamsList a ~ (Last Int, GistParams a) => String -> GistParamsList a
  parseParamList s = (mempty, parseParam @a s) <> case words s of
    ["show-first", n] -> case readMaybe n of
      Just n' -> (pure n', mempty)
      Nothing -> mempty
    _ -> mempty

instance Gist () where
  type GistParams () = ()

instance Gist Int where
  type GistParams Int = ()

instance Gist Double where
  type GistParams Double = Last Printf.FieldFormat
  gist' = \case
    Last Nothing  -> pretty
    Last (Just f) -> \d -> pretty $ Printf.formatRealFloat d f ""

  parseParam = fromMaybe mempty . go
   where
    -- This is pretty awkward. Text.Printf doesn't expose any way to parse a
    -- FieldFormat, so we do it ourselves. We might want our own version of
    -- FieldFormat, to get a Show instance and to allow comma separation.
    go = \case
      [] -> Nothing
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
        width <- if null widthS then Just Nothing else Just <$> readMaybe widthS
        let (mPrecS, rest2) = case rest1 of
              ('.' : s') -> first Just $ span isDigit s'
              _          -> (Nothing, rest1)
        prec <- case mPrecS of
          Nothing    -> Just Nothing
          Just ""    -> Just (Just 0)
          Just precS -> Just <$> readMaybe precS
        case rest2 of
          (c : []) | isFmtChar c -> Just $ pure $ Printf.FieldFormat
            { Printf.fmtWidth     = width
            , Printf.fmtPrecision = prec
            , Printf.fmtAdjust    = Nothing
            , Printf.fmtSign      = Nothing
            , Printf.fmtAlternate = False
            , Printf.fmtModifiers = ""
            , Printf.fmtChar      = c
            }
          _ -> Nothing

    a <&&> f = fmap (fmap f) a

data GPStrQuotes
  = GPStrQuotesAlways
  | GPStrQuotesNever
  | GPStrQuotesSometimes
  deriving stock (Eq, Show)

-- | If we use `GPStrQuotesSometimes`, this decides whether an individual
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

parseParamGPStrQuotes :: String -> Last GPStrQuotes
parseParamGPStrQuotes = \case
  "QuotesAlways"    -> pure GPStrQuotesAlways
  "QuotesNever"     -> pure GPStrQuotesNever
  "QuotesSometimes" -> pure GPStrQuotesSometimes
  _                 -> mempty

instance Gist Char where
  type GistParams Char = Last GPStrQuotes
  gist' q c = case fromLast GPStrQuotesSometimes q of
    GPStrQuotesAlways    -> viaShow c
    GPStrQuotesNever     -> pretty c
    GPStrQuotesSometimes -> if charWantsQuotes c then viaShow c else pretty c
  parseParam = parseParamGPStrQuotes

  type GistParamsList Char = Last GPStrQuotes
  gistList' q s = gist' q (Text.pack s)
  parseParamList = parseParamGPStrQuotes

instance Gist Text where
  type GistParams Text = Last GPStrQuotes
  gist' q = case fromLast GPStrQuotesSometimes q of
    GPStrQuotesAlways -> viaShow
    GPStrQuotesNever  -> pretty
    GPStrQuotesSometimes ->
      \t -> if Text.any charWantsQuotes t then viaShow t else pretty t
  parseParam = parseParamGPStrQuotes

instance (Gist a, Gist b) => Gist (a, b) where
  type GistParams (a, b) = (GistParams a, GistParams b)
  gist' (pa, pb) (a, b) = tupled [gist' pa a, gist' pb b]
  parseParam s = (parseParam @a s, parseParam @b s)

instance Gist a => Gist [a] where
  type GistParams [a] = GistParamsList a
  gist'      = gistList'
  parseParam = parseParamList @a

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

  parseParam s = (mempty, parseParam @k s, parseParam @v s) <> case s of
    "hide-keys" -> (GPMap (pure False) mempty, mempty, mempty)
    "hide-vals" -> (GPMap mempty (pure False), mempty, mempty)
    _           -> mempty

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x
