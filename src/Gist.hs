module Gist
  ( Gist(..)
  , GPMap(..)
  , GPStrQuotes(..)
  , runParamParser
  ) where

import           Data.Bifunctor                 ( first
                                                , second
                                                )
import qualified Data.Char                     as Char
import           Data.Functor                   ( (<&>) )
import           Data.Kind                      ( Type )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Monoid                    ( Last(..) )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Traversable               ( for )
import           Prettyprinter
import qualified Text.Printf                   as Printf
import           Text.Read                      ( readMaybe )

class (Monoid (GistParams a), Monoid (GistParamsList a)) => Gist a where
  type GistParams a :: Type

  gist' :: GistParams a -> a -> Doc ann
  default gist' :: (GistParams a ~ (), Pretty a) => GistParams a -> a -> Doc ann
  gist' ~() = pretty
  -- ~() here doesn't force, but does avoid a redundant constraint warning

  paramParser :: ParamParser (GistParams a)
  paramParser = mempty

  gist :: a -> Doc ann
  gist = gist' mempty

  gistP :: [(String, String)] -> a -> Doc ann
  gistP args = gist' $ either error id (runParamParser (paramParser @a) args)

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
  paramParserList :: ParamParser (GistParamsList a)
  default paramParserList
    :: GistParamsList a ~ (Last Int, GistParams a)
    => ParamParser (GistParamsList a)
  paramParserList =
    ((mempty, ) <$> paramParser @a) <> ((, mempty) <$> listParser)
   where
    listParser = ParamParser $ Map.singleton "[]" $ \s -> case words s of
      ["show-first", n] -> case readMaybe n of
        Just n' -> Right (pure n')
        Nothing -> Left "Expected \"show-first (int)\""
      _ -> Left "Expected \"show-first (int)\""

newtype ParamParser a = ParamParser (Map String (String -> Either String a))
  deriving stock Functor

instance Semigroup a => Semigroup (ParamParser a) where
  ParamParser m1 <> ParamParser m2 = ParamParser $ Map.unionWith combine m1 m2
    where f1 `combine` f2 = \s -> (<>) <$> f1 s <*> f2 s

instance Monoid a => Monoid (ParamParser a) where
  mempty = ParamParser mempty

runParamParser
  :: Monoid a => ParamParser a -> [(String, String)] -> Either String a
runParamParser (ParamParser parser) args =
  second mconcat $ for args $ \(ctx, p) -> if ctx == "*"
    then Right $ mconcat $ Map.elems $ parser <&> \func ->
      either mempty id (func p)
    else case Map.lookup ctx parser of
      Nothing   -> Right mempty
      Just func -> func p

instance Gist () where
  type GistParams () = ()

instance Gist Int where
  type GistParams Int = ()

instance Gist Double where
  type GistParams Double = Last Printf.FieldFormat
  gist' = \case
    Last Nothing  -> pretty
    Last (Just f) -> \d -> pretty $ Printf.formatRealFloat d f ""

  paramParser = ParamParser $ Map.singleton "Double" go
   where
    -- This is pretty awkward. Text.Printf doesn't expose any way to parse a
    -- FieldFormat, so we do it ourselves. We might want our own version of
    -- FieldFormat, to get a Show instance and to allow comma separation.
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

paramParserGPStrQuotes :: ParamParser (Last GPStrQuotes)
paramParserGPStrQuotes = ParamParser $ Map.singleton "string" $ \case
  "QuotesAlways"    -> Right $ pure GPStrQuotesAlways
  "QuotesNever"     -> Right $ pure GPStrQuotesNever
  "QuotesSometimes" -> Right $ pure GPStrQuotesSometimes
  _                 -> Left "unknown quote specifier"

instance Gist Char where
  type GistParams Char = Last GPStrQuotes
  gist' q c = case fromLast GPStrQuotesSometimes q of
    GPStrQuotesAlways    -> viaShow c
    GPStrQuotesNever     -> pretty c
    GPStrQuotesSometimes -> if charWantsQuotes c then viaShow c else pretty c
  paramParser = paramParserGPStrQuotes

  type GistParamsList Char = Last GPStrQuotes
  gistList' q s = gist' q (Text.pack s)
  paramParserList = paramParserGPStrQuotes

instance Gist Text where
  type GistParams Text = Last GPStrQuotes
  gist' q = case fromLast GPStrQuotesSometimes q of
    GPStrQuotesAlways -> viaShow
    GPStrQuotesNever  -> pretty
    GPStrQuotesSometimes ->
      \t -> if Text.any charWantsQuotes t then viaShow t else pretty t
  paramParser = paramParserGPStrQuotes

instance (Gist a, Gist b) => Gist (a, b) where
  type GistParams (a, b) = (GistParams a, GistParams b)
  gist' (pa, pb) (a, b) = tupled [gist' pa a, gist' pb b]
  paramParser =
    ((, mempty) <$> paramParser @a) <> ((mempty, ) <$> paramParser @b)

instance Gist a => Gist [a] where
  type GistParams [a] = GistParamsList a
  gist'       = gistList'
  paramParser = paramParserList @a

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

  paramParser =
    ((mempty, , mempty) <$> paramParser @k)
      <> ((mempty, mempty, ) <$> paramParser @v)
      <> ((, mempty, mempty) <$> mapParser)
   where
    mapParser = ParamParser $ Map.singleton "Map" $ \case
      "hide-keys" -> Right $ GPMap (pure False) mempty
      "hide-vals" -> Right $ GPMap mempty (pure False)
      _           -> Left "Expected hide-keys or hide-vals"

fromLast :: a -> Last a -> a
fromLast def = \case
  Last Nothing  -> def
  Last (Just x) -> x
