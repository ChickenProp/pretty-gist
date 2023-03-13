module Main where

import           Data.Foldable                  ( for_ )
import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Text     as PPText
import qualified Test.Hspec                    as Hspec
import           Test.Hspec                     ( describe
                                                , it
                                                )
import           Test.Hspec.Expectations
import qualified Test.QuickCheck               as QC

import           Gist                           ( gistP )

main :: IO ()
main = Hspec.hspec $ do
  let
    layout n = PPText.renderStrict . PP.layoutPretty
      (PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine n 1 })

  describe "gisting Double" $ do
    let tests =
          [ ([]       , 123.4, "123.4")
          , ([".3f"]  , 123.4, "123.400")
          , (["7.3f"] , 123.4, "123.400")
          , (["8.3f"] , 123.4, " 123.400")
          , (["08.3f"], 123.4, "0123.400")
          ]
    for_ tests $ \(args, input :: Double, expected) -> do
      let desc = concat [show args, " / ", show input, " === ", show expected]
      it desc $ do
        QC.forAll
            ((,) <$> QC.arbitrary <*> QC.elements ["Double", "Floating", "*"])
          $ \(width, context) ->
              layout width (gistP ((context, ) <$> args) input)
                `shouldBe` expected

  describe "gisting String" $ do
    let tests =
          [ (["QuotesAlways"]   , "foo"    , "\"foo\"")
          , (["QuotesSometimes"], "foo"    , "foo")
          , (["QuotesNever"]    , "foo"    , "foo")
          , (["QuotesAlways"]   , "foo bar", "\"foo bar\"")
          , (["QuotesSometimes"], "foo bar", "\"foo bar\"")
          , (["QuotesNever"]    , "foo bar", "foo bar")
          ]
    for_ tests $ \(args, input :: String, expected) -> do
      let desc = concat [show args, " / ", show input, " === ", show expected]
      it desc $ do
        QC.forAll
            ((,) <$> QC.arbitrary <*> QC.elements ["IsString", "String", "*"])
          $ \(width, context) ->
              layout width (gistP ((context, ) <$> args) input)
                `shouldBe` expected
