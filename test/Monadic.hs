module Monadic
  ( spec
  ) where

import           Control.Lens
import           Data.Bool                      ( bool )
import           Data.Foldable                  ( for_ )
import           Data.Generics.Product          ( field
                                                , field'
                                                )
import qualified Data.IORef                    as IORef
import           Data.Monoid                    ( Last )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Text     as PPText
import qualified Test.Hspec                    as Hspec
import           Test.Hspec                     ( describe
                                                , it
                                                )
import qualified Test.Hspec.Core.Spec          as Hspec.Spec
import           Test.Hspec.Expectations
import qualified Test.QuickCheck               as QC

import qualified Gist.Monadic                  as Gist
import           Gist.Monadic                   ( Gist(..)
                                                , gist
                                                )

spec :: Hspec.Spec
spec = do
  let
    layout n = PPText.renderStrict . PP.layoutPretty
      (PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine n 1 })

  describe "gisting floaty things" $ do
    let tests :: Floating a => [(Maybe String, a, Text)]
        tests =
          [ (Nothing      , 123.4, "123.4")
          , (Just "%.0f"  , 123.4, "123")
          , (Just "%.3f"  , 123.4, "123.400")
          , (Just "%7.3f" , 123.4, "123.400")
          , (Just "%8.3f" , 123.4, " 123.400")
          , (Just "%08.3f", 123.4, "0123.400")
          ]

    describe "Double" $ do
      for_ tests $ \(arg, input :: Double, expected) -> do
        let desc = concat [show arg, " / ", show input, " === ", show expected]
        it desc $ do
          QC.forAll
              ((,) <$> QC.arbitrary <*> QC.elements
                [ ("Double"  , QC.Blind $ Gist.configF @Double)
                , ("Floating", QC.Blind $ Gist.configF @Floating)
                ]
              )
            $ \(width, (_ :: String, QC.Blind context)) ->
                layout
                    width
                    (gist (context $ \c -> c { Gist.printfFmt = pure arg })
                          input
                    )
                  `shouldBe` expected

    describe "Float" $ do
      for_ tests $ \(arg, input :: Float, expected) -> do
        let desc = concat [show arg, " / ", show input, " === ", show expected]
        it desc $ do
          QC.forAll
              ((,) <$> QC.arbitrary <*> QC.elements
                [ ("Float"   , QC.Blind $ Gist.configF @Float)
                , ("Floating", QC.Blind $ Gist.configF @Floating)
                ]
              )
            $ \(width, (_ :: String, QC.Blind context)) ->
                layout
                    width
                    (gist (context $ \c -> c { Gist.printfFmt = pure arg })
                          input
                    )
                  `shouldBe` expected

    describe "MyFloat" $ do
      for_ tests $ \(arg, input :: Gist.MyFloat, expected) -> do
        let desc = concat [show arg, " / ", show input, " === ", show expected]
        it desc $ do
          QC.forAll
              ((,) <$> QC.arbitrary <*> QC.elements
                [ ("MyFloat" , QC.Blind $ Gist.configF @Gist.MyFloat)
                , ("Floating", QC.Blind $ Gist.configF @Floating)
                ]
              )
            $ \(width, (_ :: String, QC.Blind context)) ->
                layout
                    width
                    (gist (context $ \c -> c { Gist.printfFmt = pure arg })
                          input
                    )
                  `shouldBe` expected
