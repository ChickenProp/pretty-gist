module ChessBoard where

import qualified Data.Char                     as Char
import qualified Prettyprinter                 as PP

data Player = Black | White
  deriving stock Show
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
  deriving stock Show

data Piece = Piece
  { pieceType :: PieceType
  , owner     :: Player
  , lastMoved :: Maybe Int
  }
  deriving stock Show

prettyPieceChar :: Piece -> PP.Doc ann
prettyPieceChar (Piece {..}) =
  PP.pretty
    $ (case owner of
        White -> id
        Black -> Char.toLower
      )
    $ case pieceType of
        King   -> 'K'
        Queen  -> 'Q'
        Rook   -> 'R'
        Bishop -> 'B'
        Knight -> 'N'
        Pawn   -> 'P'

newtype Board a = Board [[a]]
  deriving stock Show

data GameState = GameState
  { turn      :: Player
  , pBlackWin :: Float
  , pWhiteWin :: Float
  , nMoves    :: Int
  , board     :: Board (Maybe Piece)
  }
  deriving stock Show

startPos :: GameState
startPos = GameState
  { turn      = White
  , pBlackWin = 0.3463
  , pWhiteWin = 0.3896
  , nMoves    = 0
  , board     = Board
                  [ (black <$> pieces)
                  , (black <$> pawns)
                  , empty
                  , empty
                  , empty
                  , empty
                  , (white <$> pawns)
                  , (white <$> pieces)
                  ]
  }
 where
  black t = Just $ Piece t Black Nothing
  white t = Just $ Piece t White Nothing
  pieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
  pawns  = replicate 8 Pawn
  empty  = replicate 8 Nothing


