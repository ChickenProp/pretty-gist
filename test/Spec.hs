module Main where

import           Test.Hspec                     ( describe
                                                , hspec
                                                )

import qualified Dynamic

main :: IO ()
main = hspec $ do
  describe "Dynamic" $ Dynamic.spec
