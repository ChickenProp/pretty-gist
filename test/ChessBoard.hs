module ChessBoard where

import qualified Data.Char                     as Char
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
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

renderedShort :: Text
renderedShort = Text.intercalate
  "\n"
  [ "GameState { turn = White"
  , "          , pBlackWin = 0.3463"
  , "          , pWhiteWin = 0.3896"
  , "          , nMoves = 0"
  , "          , board = [ [r, n, b, q, k, b, n, r]"
  , "                    , [p, p, p, p, p, p, p, p]"
  , "                    , [_, _, _, _, _, _, _, _]"
  , "                    , [_, _, _, _, _, _, _, _]"
  , "                    , [_, _, _, _, _, _, _, _]"
  , "                    , [_, _, _, _, _, _, _, _]"
  , "                    , [P, P, P, P, P, P, P, P]"
  , "                    , [R, N, B, Q, K, B, N, R] ]"
  , "          }"
  ]

renderedLong :: Text
renderedLong = Text.intercalate
  "\n"
  [ "GameState { turn = White"
  , "          , pBlackWin = 0.3463"
  , "          , pWhiteWin = 0.3896"
  , "          , nMoves = 0"
  , "          , board = [ [ Piece {pieceType = Rook, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Knight, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Bishop, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Queen, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = King, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Bishop, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Knight, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Rook, owner = Black, lastMoved = _} ]"
  , "                    , [ Piece {pieceType = Pawn, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = Black, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = Black, lastMoved = _} ]"
  , "                    , [_, _, _, _, _, _, _, _]"
  , "                    , [_, _, _, _, _, _, _, _]"
  , "                    , [_, _, _, _, _, _, _, _]"
  , "                    , [_, _, _, _, _, _, _, _]"
  , "                    , [ Piece {pieceType = Pawn, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Pawn, owner = White, lastMoved = _} ]"
  , "                    , [ Piece {pieceType = Rook, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Knight, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Bishop, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Queen, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = King, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Bishop, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Knight, owner = White, lastMoved = _}"
  , "                      , Piece {pieceType = Rook, owner = White, lastMoved = _"
  , "                              } ] ]"
  , "          }"
  ]

renderedFull :: Text
renderedFull = Text.intercalate
  "\n"
  [ "GameState { turn = White"
  , "          , pBlackWin = 0.3463"
  , "          , pWhiteWin = 0.3896"
  , "          , nMoves = 0"
  , "          , board = [ [ Just (Piece { pieceType = Rook"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Knight"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Bishop"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Queen"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = King"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Bishop"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Knight"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Rook"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    }) ]"
  , "                    , [ Just (Piece { pieceType = Pawn"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = Black"
  , "                                    , lastMoved = Nothing"
  , "                                    }) ]"
  , "                    , [ Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing ]"
  , "                    , [ Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing ]"
  , "                    , [ Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing ]"
  , "                    , [ Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing"
  , "                      , Nothing ]"
  , "                    , [ Just (Piece { pieceType = Pawn"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Pawn"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    }) ]"
  , "                    , [ Just (Piece { pieceType = Rook"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Knight"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Bishop"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Queen"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = King"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Bishop"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Knight"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    })"
  , "                      , Just (Piece { pieceType = Rook"
  , "                                    , owner = White"
  , "                                    , lastMoved = Nothing"
  , "                                    }) ] ]"
  , "          }"
  ]
