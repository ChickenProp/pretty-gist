module ChessBoard.Classless
  ( module ChessBoard
  , module ChessBoard.Classless
  ) where

import           ChessBoard
import qualified Gist.Classless                as Gist
import           Gist.Classless                 ( Prec )
import           Prettyprinter

data ConfigPiece = ConfigPiece
  { singleChar      :: Bool
  , renderPieceType :: forall ann . Prec -> PieceType -> Doc ann
  , renderOwner     :: forall ann . Prec -> Player -> Doc ann
  , renderLastMoved :: forall ann . Prec -> Maybe Int -> Doc ann
  }

defaultConfigPiece :: ConfigPiece
defaultConfigPiece = ConfigPiece
  { singleChar      = False
  , renderPieceType = Gist.gistShowily
  , renderOwner     = Gist.gistShowily
  , renderLastMoved = Gist.gistMaybe
                        Gist.defaultConfigMaybe
                        (const $ Gist.gistPrintfily Gist.defaultConfigPrintf)
  }

gistPiece :: ConfigPiece -> Prec -> Piece -> Doc ann
gistPiece (ConfigPiece {..}) prec piece@(Piece {..}) = if singleChar
  then prettyPieceChar piece
  else Gist.record
    prec
    (Just "Piece")
    [ ("pieceType", renderPieceType 0 pieceType)
    , ("owner"    , renderOwner 0 owner)
    , ("lastMoved", renderLastMoved 0 lastMoved)
    ]

gistBoard :: (Prec -> [[a]] -> Doc ann) -> Prec -> Board a -> Doc ann
gistBoard renderer prec (Board a) = renderer prec a

data ConfigGameState = ConfigGameState
  { renderTurn      :: forall ann . Prec -> Player -> Doc ann
  , renderPBlackWin :: forall ann . Prec -> Float -> Doc ann
  , renderPWhiteWin :: forall ann . Prec -> Float -> Doc ann
  , renderNMoves    :: forall ann . Prec -> Int -> Doc ann
  , renderBoard     :: forall ann . Prec -> Board (Maybe Piece) -> Doc ann
  }

defaultConfigGameState :: ConfigGameState
defaultConfigGameState = ConfigGameState
  { renderTurn      = Gist.gistShowily
  , renderPBlackWin = const $ Gist.gistPrintfily Gist.defaultConfigPrintf
  , renderPWhiteWin = const $ Gist.gistPrintfily Gist.defaultConfigPrintf
  , renderNMoves    = const $ Gist.gistPrintfily Gist.defaultConfigPrintf
  , renderBoard     = gistBoard
                      $ Gist.gistList Gist.defaultConfigList
                      $ Gist.gistList Gist.defaultConfigList
                      $ Gist.gistMaybe Gist.defaultConfigMaybe
                      $ gistPiece
                      $ defaultConfigPiece { singleChar = True }
  }

gistGameState :: ConfigGameState -> Prec -> GameState -> Doc ann
gistGameState (ConfigGameState {..}) prec (GameState {..}) = Gist.record
  prec
  (Just "GameState")
  [ ("turn"     , renderTurn 0 turn)
  , ("pBlackWin", renderPBlackWin 0 pBlackWin)
  , ("pWhiteWin", renderPWhiteWin 0 pWhiteWin)
  , ("nMoves"   , renderNMoves 0 nMoves)
  , ("board"    , renderBoard 0 board)
  ]
