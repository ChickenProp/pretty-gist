module Main where

import           Test.Hspec                     ( describe
                                                , hspec
                                                )

import qualified Classless
import qualified Dynamic
import qualified Monadic
import qualified Simple

main :: IO ()
main = hspec $ do
  describe "Classless" $ Classless.spec
  describe "Simple" $ Simple.spec
  describe "Dynamic" $ Dynamic.spec
  describe "Monadic" $ Monadic.spec
