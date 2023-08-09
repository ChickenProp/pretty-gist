module Main where

import           Test.Hspec                     ( describe
                                                , hspec
                                                )

import qualified Classless
import qualified Dynamic
import qualified OneClass
import qualified TwoClass

main :: IO ()
main = hspec $ do
  describe "Classless" $ Classless.spec
  describe "OneClass" $ OneClass.spec
  describe "Dynamic" $ Dynamic.spec
  describe "TwoClass" $ TwoClass.spec
