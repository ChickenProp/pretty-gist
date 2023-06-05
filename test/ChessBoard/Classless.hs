module ChessBoard.Classless
  ( module ChessBoard
  , module ChessBoard.Classless
  ) where

import           ChessBoard
import           Data.Maybe                     ( isJust )
import           GHC.Generics                   ( Generic )
import           Prettyprinter
import qualified Text.Printf                   as Printf

parensIf :: Bool -> Doc ann -> Doc ann
parensIf cond = if cond then parens else id

record :: Int -> Maybe (Doc ann) -> [(Doc ann, Doc ann)] -> Doc ann
record prec mConstr fields =
  parensIf (prec > 10 && isJust mConstr)
    $ maybe id (\constr contents -> constr <+> align contents) mConstr
    $ group
    $ encloseSep (flatAlt "{ " "{") (flatAlt "\n}" "}") ", "
    $ flip map fields
    $ \(key, val) -> key <+> "=" <+> val

data ConfigMaybe = ConfigMaybe
  { showConstructors :: Bool
  }
  deriving stock Generic

defaultConfigMaybe :: ConfigMaybe
defaultConfigMaybe = ConfigMaybe { showConstructors = False }

gistMaybe :: ConfigMaybe -> (Int -> a -> Doc ann) -> Int -> Maybe a -> Doc ann
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

gistShowily :: Show a => Int -> a -> Doc ann
gistShowily prec a = pretty $ showsPrec prec a ""

data ConfigPiece = ConfigPiece
  { singleChar      :: Bool
  , renderPieceType :: forall ann . Int -> PieceType -> Doc ann
  , renderOwner     :: forall ann . Int -> Player -> Doc ann
  , renderLastMoved :: forall ann . Int -> Maybe Int -> Doc ann
  }

defaultConfigPiece :: ConfigPiece
defaultConfigPiece = ConfigPiece
  { singleChar      = False
  , renderPieceType = gistShowily
  , renderOwner     = gistShowily
  , renderLastMoved = gistMaybe defaultConfigMaybe
                                (const $ gistPrintfily defaultConfigPrintf)
  }

gistPiece :: ConfigPiece -> Int -> Piece -> Doc ann
gistPiece (ConfigPiece {..}) prec piece@(Piece {..}) = if singleChar
  then prettyPieceChar piece
  else record
    prec
    (Just "Piece")
    [ ("pieceType", renderPieceType 0 pieceType)
    , ("owner"    , renderOwner 0 owner)
    , ("lastMoved", renderLastMoved 0 lastMoved)
    ]

gistBoard :: (Int -> [[a]] -> Doc ann) -> Int -> Board a -> Doc ann
gistBoard renderer prec (Board a) = renderer prec a

data ConfigGameState = ConfigGameState
  { renderTurn      :: forall ann . Int -> Player -> Doc ann
  , renderPBlackWin :: forall ann . Int -> Float -> Doc ann
  , renderPWhiteWin :: forall ann . Int -> Float -> Doc ann
  , renderNMoves    :: forall ann . Int -> Int -> Doc ann
  , renderBoard     :: forall ann . Int -> Board (Maybe Piece) -> Doc ann
  }

defaultConfigGameState :: ConfigGameState
defaultConfigGameState = ConfigGameState
  { renderTurn      = gistShowily
  , renderPBlackWin = const $ gistPrintfily defaultConfigPrintf
  , renderPWhiteWin = const $ gistPrintfily defaultConfigPrintf
  , renderNMoves    = const $ gistPrintfily defaultConfigPrintf
  , renderBoard     = undefined
  }

-- gistGameState :: ConfigGameState -> Int -> GameState -> Doc ann
