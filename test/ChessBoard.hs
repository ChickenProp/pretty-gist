module ChessBoard where

import qualified Data.Char                     as Char
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Prettyprinter                 as PP

import qualified Gist                          as Gist
import           Gist                           ( Gist(..) )
import qualified Gist                          as Gist.ConfigList
                                                ( ConfigList(..) )
import qualified Gist                          as Gist.ConfigMaybe
                                                ( ConfigMaybe(..) )

data Player
  = Black
  | White
  deriving stock Show
  deriving Gist via Gist.Showily Player

data PieceType
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  deriving stock Show
  deriving Gist via Gist.Showily PieceType

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

data ConfigPiece = ConfigPiece
  { singleChar    :: Bool
  , gistPieceType :: Gist.Gister PieceType
  , gistOwner     :: Gist.Gister Player
  , gistLastMoved :: Gist.Gister (Maybe Int)
  }
  deriving stock Generic

instance Gist Piece where
  type Config Piece = ConfigPiece
  defaultConfig =
    ConfigPiece False
                (Gist.ConfGister $ defaultConfig @PieceType)
                (Gist.ConfGister $ defaultConfig @Player)
                (Gist.ConfGister $ defaultConfig @(Maybe Int))

  gistPrec prec (ConfigPiece {..}) piece@(Piece {..}) = if singleChar
    then prettyPieceChar piece
    else
      Gist.record
        prec
        (Just "Piece")
        [ ("pieceType", Gist.runGister gistPieceType pieceType)
        , ("owner"    , Gist.runGister gistOwner owner)
        , ("lastMoved", Gist.runGister gistLastMoved lastMoved)
        ]

newtype Board a = Board [[a]]
  deriving stock Show
  deriving newtype Gist

data GameState = GameState
  { turn      :: Player
  , pBlackWin :: Float
  , pWhiteWin :: Float
  , nMoves    :: Int
  , board     :: Board (Maybe Piece)
  }
  deriving stock Show

data ConfigGameState = ConfigGameState
  { gistTurn      :: Gist.Gister Player
  , gistPBlackWin :: Gist.Gister Float
  , gistPWhiteWin :: Gist.Gister Float
  , gistNMoves    :: Gist.Gister Int
  , gistBoard     :: Gist.Gister (Board (Maybe Piece))
  }
  deriving stock Generic

instance Gist GameState where
  type Config GameState = ConfigGameState
  defaultConfig = ConfigGameState
    { gistTurn      = Gist.defaultConfGister
    , gistPBlackWin = Gist.defaultConfGister
    , gistPWhiteWin = Gist.defaultConfGister
    , gistNMoves    = Gist.defaultConfGister
    , gistBoard     =
      let
        gPiece  = Gist.defaultConfGisterF $ \c -> c { singleChar = True }
        gMPiece = Gist.defaultConfGisterF $ \c ->
          c { Gist.ConfigMaybe.gistElem = gPiece }
        gLMPiece = Gist.defaultConfGisterF $ \c ->
          c { Gist.ConfigList.gistElem = gMPiece }
        gBoard = Gist.defaultConfGisterF $ \c ->
          c { Gist.ConfigList.gistElem = gLMPiece }
      in
        gBoard
    }

  gistPrec prec (ConfigGameState {..}) (GameState {..}) =
    Gist.record
      prec
      (Just "GameState")
      [ ("turn"     , Gist.runGister gistTurn turn)
      , ("pBlackWin", Gist.runGister gistPBlackWin pBlackWin)
      , ("pWhiteWin", Gist.runGister gistPWhiteWin pWhiteWin)
      , ("nMoves"   , Gist.runGister gistNMoves nMoves)
      , ("board"    , Gist.runGister gistBoard board)
      ]

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
renderedShort =
  Text.intercalate
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
renderedLong =
  Text.intercalate
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
renderedFull =
  Text.intercalate
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
