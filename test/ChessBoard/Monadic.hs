{-# OPTIONS_GHC -Wno-orphans #-}

module ChessBoard.Monadic
  ( module ChessBoard
  , module ChessBoard.Monadic
  ) where

import           ChessBoard
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Monoid                    ( Last(..) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import qualified Gist.Monadic                  as Gist
import           Gist.Monadic                   ( Gist(..)
                                                , fromLast
                                                )

deriving via Gist.Showily Player instance Gist.Configurable Player
deriving via Gist.Showily Player instance Gist Player

deriving via Gist.Showily PieceType instance Gist.Configurable PieceType
deriving via Gist.Showily PieceType instance Gist PieceType

-- Can't derive an instance for `Board`. We have that `Board a` is
-- representation-equivalent to `[[a]]`, but `Board` itself isn't
-- representation-equivalent to anything. Anyway, even if we had an instance,
-- the instance we've derived for `Gist (Board a)` wouldn't look at it.
deriving newtype instance Typeable a => Gist.Configurable (Board a)
deriving newtype instance Gist a => Gist (Board a)

data ConfigPiece f = ConfigPiece
  { singleChar :: f Bool
  }
  deriving stock Generic
instance Semigroup (ConfigPiece Last) where
  (ConfigPiece a1) <> (ConfigPiece a2) = ConfigPiece (a1 <> a2)
instance Monoid (ConfigPiece Last) where
  mempty = ConfigPiece mempty

instance Gist.Configurable Piece where
  type ConfigFor Piece f = ConfigPiece f

instance Gist Piece where
  type GistPathComponents Piece = ()
  reifyConfig (ConfigPiece a) = ConfigPiece (Identity $ fromLast False a)
  renderM prec (ConfigPiece {..}) piece@(Piece {..}) =
    if runIdentity singleChar
      then pure $ prettyPieceChar piece
      else Gist.record
        prec
        (Just "Piece")
        [ ("pieceType", Gist.subGist (Just "pieceType") pieceType)
        , ("owner"    , Gist.subGist (Just "owner") owner)
        , ("lastMoved", Gist.subGist (Just "lastMoved") lastMoved)
        ]

instance Gist.Configurable GameState where
  type ConfigFor GameState f = Proxy f

instance Gist GameState where
  type GistPathComponents GameState = ()
  reifyConfig _ = Proxy
  renderM prec _ (GameState {..}) =
    Gist.localPushConf
        (Gist.configF @Piece $ \c -> c { singleChar = pure True })
      $ Gist.record
          prec
          (Just "GameState")
          [ ("turn"     , Gist.subGist (Just "turn") turn)
          , ("pBlackWin", Gist.subGist (Just "pBlackWin") pBlackWin)
          , ("pWhiteWin", Gist.subGist (Just "pWhiteWin") pWhiteWin)
          , ("nMoves"   , Gist.subGist (Just "nMoves") nMoves)
          , ("board"    , Gist.subGist (Just "board") board)
          ]
