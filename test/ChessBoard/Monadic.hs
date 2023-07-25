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
import           Gist.Monadic                   ( Configurable(..)
                                                , Gist(..)
                                                , fromLast
                                                )

deriving via Gist.Showily Player instance Configurable Player
deriving via Gist.Showily Player instance Gist Player

deriving via Gist.Showily PieceType instance Configurable PieceType
deriving via Gist.Showily PieceType instance Gist PieceType

-- We can't derive an instance `Configurable Board`. We have that `Board a` is
-- representation-equivalent to `[[a]]`, but `Board` itself isn't
-- representation-equivalent to anything. Anyway, even if we had an instance,
-- the instance we're deriving for `Gist (Board a)` wouldn't look at it.
deriving newtype instance Typeable a => Configurable (Board a)
deriving newtype instance Gist a => Gist (Board a)

data ConfigPiece f = ConfigPiece
  { singleChar :: f Bool
  }
  deriving stock Generic
instance Semigroup (ConfigPiece Last) where
  (ConfigPiece a1) <> (ConfigPiece a2) = ConfigPiece (a1 <> a2)
instance Monoid (ConfigPiece Last) where
  mempty = ConfigPiece mempty

instance Configurable Piece where
  type ConfigFor Piece f = ConfigPiece f

instance Gist Piece where
  type GistLookups Piece = ()
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

instance Configurable GameState where
  type ConfigFor GameState f = Proxy f

instance Gist GameState where
  type GistLookups GameState = ()
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
