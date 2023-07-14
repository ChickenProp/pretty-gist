module Classless
  ( spec
  ) where

import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Text     as PPText
import qualified Test.Hspec                    as Hspec
import           Test.Hspec                     ( it )
import           Test.Hspec.Expectations

import qualified ChessBoard.Classless          as CB
import qualified Gist.Classless                as Gist

spec :: Hspec.Spec
spec = do
  let
    layout n = PPText.renderStrict . PP.layoutPretty
      (PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine n 1 })

  it "Short" $ do
    layout 80 (CB.gistGameState CB.defaultConfigGameState 0 CB.startPos)
      `shouldBe` CB.renderedShort

  it "Long" $ do
    let conf = CB.defaultConfigGameState
          { CB.renderBoard = CB.gistBoard
                             $ Gist.gistList Gist.defaultConfigList
                             $ Gist.gistList Gist.defaultConfigList
                             $ Gist.gistMaybe Gist.defaultConfigMaybe
                             $ CB.gistPiece
                             $ CB.defaultConfigPiece { CB.singleChar = False }
          }
    layout 80 (CB.gistGameState conf 0 CB.startPos) `shouldBe` CB.renderedLong
