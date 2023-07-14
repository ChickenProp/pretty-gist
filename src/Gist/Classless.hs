module Gist.Classless
  ( Prec(..)
  , ConfigList(..)
  , defaultConfigList
  , gistList
  , ConfigMaybe(..)
  , defaultConfigMaybe
  , gistMaybe
  , defaultConfigPrintf
  , gistPrintfily
  , gistShowily
  , parensIf
  , record
  ) where

import           Data.Maybe                     ( isJust )
import           GHC.Generics                   ( Generic )
import           Prettyprinter
import qualified Text.Printf                   as Printf

newtype Prec = Prec Int
  deriving newtype (Eq, Ord, Num)

data ConfigList = ConfigList
  { showFirst :: Maybe Int
  }
  deriving stock Generic

defaultConfigList :: ConfigList
defaultConfigList = ConfigList { showFirst = Nothing }

gistList :: ConfigList -> (Prec -> a -> Doc ann) -> Prec -> [a] -> Doc ann
gistList (ConfigList {..}) renderElem _ xs =
  let elems = case showFirst of
        Nothing -> renderElem 0 <$> xs
        Just n  -> case splitAt n xs of
          (start, []   ) -> renderElem 0 <$> start
          (start, _ : _) -> (renderElem 0 <$> start) ++ ["..."]
  in  align $ list elems

data ConfigMaybe = ConfigMaybe
  { showConstructors :: Bool
  }
  deriving stock Generic

defaultConfigMaybe :: ConfigMaybe
defaultConfigMaybe = ConfigMaybe { showConstructors = False }

gistMaybe
  :: ConfigMaybe -> (Prec -> a -> Doc ann) -> Prec -> Maybe a -> Doc ann
gistMaybe (ConfigMaybe {..}) renderElem prec = if showConstructors
  then \case
    Nothing -> "Nothing"
    Just a  -> parensIf (prec > 10) $ "Just" <+> renderElem 11 a
  else \case
    Nothing -> "_"
    Just a  -> renderElem prec a

data ConfigPrintf = ConfigPrintf
  { printfFmt :: Maybe String
  }
  deriving stock Generic

defaultConfigPrintf :: ConfigPrintf
defaultConfigPrintf = ConfigPrintf { printfFmt = Nothing }

gistPrintfily :: (Show a, Printf.PrintfArg a) => ConfigPrintf -> a -> Doc ann
gistPrintfily (ConfigPrintf {..}) a = case printfFmt of
  Nothing  -> viaShow a
  Just fmt -> pretty (Printf.printf fmt a :: String)

gistShowily :: Show a => Prec -> a -> Doc ann
gistShowily (Prec prec) a = pretty $ showsPrec prec a ""

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
