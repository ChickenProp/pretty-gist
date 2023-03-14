module Main where

import           Data.Foldable                  ( for_ )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Text     as PPText
import qualified Test.Hspec                    as Hspec
import           Test.Hspec                     ( describe
                                                , it
                                                )
import           Test.Hspec.Expectations
import qualified Test.QuickCheck               as QC

import qualified Gist
import           Gist                           ( gist )

main :: IO ()
main = Hspec.hspec $ do
  let
    layout n = PPText.renderStrict . PP.layoutPretty
      (PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine n 1 })

  describe "gisting floaty things" $ do
    let tests :: Floating a => [([String], a, Text)]
        tests =
          [ ([]       , 123.4, "123.4")
          , ([".3f"]  , 123.4, "123.400")
          , (["7.3f"] , 123.4, "123.400")
          , (["8.3f"] , 123.4, " 123.400")
          , (["08.3f"], 123.4, "0123.400")
          ]

    describe "Double" $ do
      for_ tests $ \(args, input :: Double, expected) -> do
        let desc =
              concat [show args, " / ", show input, " === ", show expected]
        it desc $ do
          QC.forAll
              ((,) <$> QC.arbitrary <*> QC.elements
                [ ("Double"  , QC.Blind $ Gist.strConfig @Double)
                , ("Floating", QC.Blind $ Gist.strConfig @Floating)
                ]
              )
            $ \(width, (_ :: String, QC.Blind context)) ->
                layout width (gist (context <$> args) input) `shouldBe` expected

    describe "Float" $ do
      for_ tests $ \(args, input :: Float, expected) -> do
        let desc =
              concat [show args, " / ", show input, " === ", show expected]
        it desc $ do
          QC.forAll
              ((,) <$> QC.arbitrary <*> QC.elements
                [ ("Float"   , QC.Blind $ Gist.strConfig @Float)
                , ("Floating", QC.Blind $ Gist.strConfig @Floating)
                ]
              )
            $ \(width, (_ :: String, QC.Blind context)) ->
                layout width (gist (context <$> args) input) `shouldBe` expected

  describe "gisting stringy things" $ do
    let tests :: IsString a => [([String], a, Text)]
        tests =
          [ (["QuotesAlways"]   , "foo"    , "\"foo\"")
          , (["QuotesSometimes"], "foo"    , "foo")
          , (["QuotesNever"]    , "foo"    , "foo")
          , (["QuotesAlways"]   , "foo-bar", "\"foo-bar\"")
          , (["QuotesSometimes"], "foo-bar", "foo-bar")
          , (["QuotesNever"]    , "foo-bar", "foo-bar")
          , (["QuotesAlways"]   , "foo bar", "\"foo bar\"")
          , (["QuotesSometimes"], "foo bar", "\"foo bar\"")
          , (["QuotesNever"]    , "foo bar", "foo bar")
          ]

    describe "String" $ do
      for_ tests $ \(args, input :: String, expected) -> do
        let desc =
              concat [show args, " / ", show input, " === ", show expected]
        it desc $ do
          QC.forAll
              ((,) <$> QC.arbitrary <*> QC.elements
                [ ("IsString", QC.Blind $ Gist.strConfig @IsString)
                , ("String"  , QC.Blind $ Gist.strConfig @String)
                ]
              )
            $ \(width, (_ :: String, QC.Blind context)) ->
                layout width (gist (context <$> args) input) `shouldBe` expected

    describe "Text" $ do
      for_ tests $ \(args, input :: Text, expected) -> do
        let desc =
              concat [show args, " / ", show input, " === ", show expected]
        it desc $ do
          QC.forAll
              ((,) <$> QC.arbitrary <*> QC.elements
                [ ("IsString", QC.Blind $ Gist.strConfig @IsString)
                , ("String"  , QC.Blind $ Gist.strConfig @Text)
                ]
              )
            $ \(width, (_ :: String, QC.Blind context)) ->
                layout width (gist (context <$> args) input) `shouldBe` expected
