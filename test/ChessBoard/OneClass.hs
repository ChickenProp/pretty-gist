{-# OPTIONS_GHC -Wno-orphans #-}

module ChessBoard.OneClass
  ( module ChessBoard
  , module ChessBoard.OneClass
  ) where

import           ChessBoard
import           GHC.Generics                   ( Generic )
import qualified Gist.OneClass                 as Gist
import           Gist.OneClass                  ( Gist(..) )
import qualified Gist.OneClass                 as Gist.ConfigList
                                                ( ConfigList(..) )
import qualified Gist.OneClass                 as Gist.ConfigMaybe
                                                ( ConfigMaybe(..) )

deriving via Gist.Showily Player instance Gist Player
deriving via Gist.Showily PieceType instance Gist PieceType
deriving newtype instance Gist (Board a)

data ConfigPiece = ConfigPiece
  { singleChar    :: Bool
  , gistPieceType :: Gist.Gister PieceType
  , gistOwner     :: Gist.Gister Player
  , gistLastMoved :: Gist.Gister (Maybe Int)
  }
  deriving stock Generic

instance Gist Piece where
  type Config Piece = ConfigPiece
  defaultConfig = ConfigPiece False
                              (Gist.ConfGister $ defaultConfig @PieceType)
                              (Gist.ConfGister $ defaultConfig @Player)
                              (Gist.ConfGister $ defaultConfig @(Maybe Int))

  gistPrec prec (ConfigPiece {..}) piece@(Piece {..}) = if singleChar
    then prettyPieceChar piece
    else Gist.record
      prec
      (Just "Piece")
      [ ("pieceType", Gist.runGister gistPieceType pieceType)
      , ("owner"    , Gist.runGister gistOwner owner)
      , ("lastMoved", Gist.runGister gistLastMoved lastMoved)
      ]

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
        gMPiece = Gist.defaultConfGisterF
          $ \c -> c { Gist.ConfigMaybe.gistElem = gPiece }
        gLMPiece = Gist.defaultConfGisterF
          $ \c -> c { Gist.ConfigList.gistElem = gMPiece }
        gBoard = Gist.defaultConfGisterF
          $ \c -> c { Gist.ConfigList.gistElem = gLMPiece }
      in
        gBoard
    }

  gistPrec prec (ConfigGameState {..}) (GameState {..}) = Gist.record
    prec
    (Just "GameState")
    [ ("turn"     , Gist.runGister gistTurn turn)
    , ("pBlackWin", Gist.runGister gistPBlackWin pBlackWin)
    , ("pWhiteWin", Gist.runGister gistPWhiteWin pWhiteWin)
    , ("nMoves"   , Gist.runGister gistNMoves nMoves)
    , ("board"    , Gist.runGister gistBoard board)
    ]
