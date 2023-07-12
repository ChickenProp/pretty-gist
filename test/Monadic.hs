module Monadic
  ( spec
  ) where

import           Control.Lens
import           Data.Foldable                  ( for_ )
import           Data.Generics.Product          ( field )
import qualified Data.IORef                    as IORef
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Text     as PPText
import qualified Test.Hspec                    as Hspec
import           Test.Hspec                     ( describe
                                                , it
                                                )
import qualified Test.Hspec.Core.Spec          as Hspec.Spec
import           Test.Hspec.Expectations
import qualified Test.QuickCheck               as QC
import           Type.Reflection                ( someTypeRep )

import qualified ChessBoard.Monadic            as CB
import qualified Gist.Monadic                  as Gist
import           Gist.Monadic                   ( gist )

spec :: Hspec.Spec
spec = do
  let
    layout n = PPText.renderStrict . PP.layoutPretty
      (PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine n 1 })

  let gist80 conf val = layout 80 (gist conf val)

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
                    (gist [context $ \c -> c { Gist.printfFmt = pure arg }]
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
                    (gist [context $ \c -> c { Gist.printfFmt = pure arg }]
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
                layout width
                       (gist [context $ field @"printfFmt" .~ pure arg] input)
                  `shouldBe` expected

  countingTests "Bunch of tests" $ \numberedTest -> do
    numberedTest $ do
      gist80 [] [(), (), ()] `shouldBe` "[(), (), ()]"

    numberedTest $ do
      gist80 [Gist.configF @[] $ field @"showFirst" .~ pure (Just 2)]
             [(), (), ()]
        `shouldBe` "[(), (), ...]"

    numberedTest $ do
      gist80 [Gist.configF @[] $ \c -> c { Gist.showFirst = pure (Just 2) }]
             [[(), (), ()], [()], [()]]
        `shouldBe` "[[(), (), ...], [()], ...]"

  countingTests "Tuple with different types" $ \numberedTest -> do
    let input      = (0.123 :: Float, [0.1 :: Float])
        expected21 = "(0.12, [0.1])"
        expected22 = "(0.12, [0.10])"

    numberedTest $ do
      gist80 [Gist.configF @Float $ field @"printfFmt" .~ pure (Just "%.2f")]
             input
        `shouldBe` expected22

    let expect21 pm = numberedTest $ do
          gist80
              [ Gist.configF @Float $ field @"printfFmt" .~ pure (Just "%.2f")
              , Gist.configPF @Float pm $ field @"printfFmt" .~ pure
                (Just "%.1f")
              ]
              input
            `shouldBe` expected21
        expect22 pm = numberedTest $ do
          gist80
              [ Gist.configF @Float $ field @"printfFmt" .~ pure (Just "%.2f")
              , Gist.configPF @Float pm $ field @"printfFmt" .~ pure
                (Just "%.1f")
              ]
              input
            `shouldBe` expected22

    expect21 $ Gist.PMTail $ (someTypeRep (Proxy @[]), []) :| []
    expect21 $ Gist.PMTail $ (someTypeRep (Proxy @[Float]), []) :| []
    expect22 $ Gist.PMTail $ (someTypeRep (Proxy @[Int]), []) :| []

    expect21
      $  Gist.PMTail
      $  (someTypeRep (Proxy @(,)), [])
      :| [(someTypeRep (Proxy @[]), [])]
    expect21
      $  Gist.PMTail
      $  (someTypeRep (Proxy @((,) Float)), [])
      :| [(someTypeRep (Proxy @[]), [])]
    expect22
      $  Gist.PMTail
      $  (someTypeRep (Proxy @((,) Int)), [])
      :| [(someTypeRep (Proxy @[]), [])]
    expect21
      $  Gist.PMExactPath
      $  (someTypeRep (Proxy @(,)), [])
      :| [(someTypeRep (Proxy @[]), [])]
    expect21
      $  Gist.PMFuzzy
      $  Gist.FCMatch (someTypeRep (Proxy @(,)), [])
      :| [Gist.FCMatch (someTypeRep (Proxy @[]), [])]
    expect21
      $  Gist.PMFuzzy
      $  Gist.FCMatch (someTypeRep (Proxy @(,)), [])
      :| [Gist.FCAny01, Gist.FCMatch (someTypeRep (Proxy @[]), [])]
    expect21
      $  Gist.PMFuzzy
      $  Gist.FCMatch (someTypeRep (Proxy @(,)), [])
      :| [Gist.FCAny0N, Gist.FCMatch (someTypeRep (Proxy @[]), [])]
    expect22
      $  Gist.PMFuzzy
      $  Gist.FCMatch (someTypeRep (Proxy @(,)), [])
      :| [Gist.FCAny11, Gist.FCMatch (someTypeRep (Proxy @[]), [])]
    expect22
      $  Gist.PMFuzzy
      $  Gist.FCMatch (someTypeRep (Proxy @(,)), [])
      :| [Gist.FCAny1N, Gist.FCMatch (someTypeRep (Proxy @[]), [])]

  countingTests "Tuple with same types" $ \numberedTest -> do
    let input      = ([0.123 :: Float], [0.1 :: Float])
        expected11 = "([0.1], [0.1])"
        expected12 = "([0.1], [0.10])"
        expected21 = "([0.12], [0.1])"
        expected22 = "([0.12], [0.10])"

    numberedTest $ do
      gist80 [Gist.configF @Float $ field @"printfFmt" .~ pure (Just "%.2f")]
             input
        `shouldBe` expected22

    let expect result pm = numberedTest $ do
          gist80
              [ Gist.configF @Float $ field @"printfFmt" .~ pure (Just "%.2f")
              , Gist.configPF @Float pm $ field @"printfFmt" .~ pure
                (Just "%.1f")
              ]
              input
            `shouldBe` result
        expect11 = expect expected11
        expect12 = expect expected12
        expect21 = expect expected21
        expect22 = expect expected22

    expect21
      $  Gist.PMExactPath
      $  (someTypeRep (Proxy @(,)), ["snd"])
      :| [(someTypeRep (Proxy @[]), [])]

    expect12
      $  Gist.PMExactPath
      $  (someTypeRep (Proxy @(,)), ["fst"])
      :| [(someTypeRep (Proxy @[]), [])]

    expect11
      $  Gist.PMExactPath
      $  (someTypeRep (Proxy @(,)), ["fst", "snd"])
      :| [(someTypeRep (Proxy @[]), [])]

    expect22
      $  Gist.PMExactPath
      $  (someTypeRep (Proxy @(,)), ["other"])
      :| [(someTypeRep (Proxy @[]), [])]

  countingTests "chessboard" $ \numberedTest -> do
    numberedTest $ do
      gist80 [] (CB.Piece CB.Pawn CB.Black Nothing)
        `shouldBe` "Piece {pieceType = Pawn, owner = Black, lastMoved = _}"

    numberedTest $ do
      layout 20 (gist [] (CB.Piece CB.Pawn CB.Black Nothing))
        `shouldBe` Text.intercalate
                     "\n"
                     [ "Piece { pieceType = Pawn"
                     , "      , owner = Black"
                     , "      , lastMoved = _"
                     , "      }"
                     ]

    numberedTest $ do
      gist80 [Gist.configF @CB.Piece $ \c -> c { CB.singleChar = pure True }]
             (CB.Piece CB.Pawn CB.Black Nothing)
        `shouldBe` "p"

    numberedTest $ do
      gist80 [] CB.startPos `shouldBe` CB.renderedShort

    numberedTest $ do
      gist80 [Gist.configF @CB.Piece $ field @"singleChar" .~ pure False]
             CB.startPos
        `shouldBe` CB.renderedLong

-- | It's a hassle to come up with descriptive test names, but convenient for
-- them all to be unique. This lets us give them numbers. We can't search for
-- the test name in source if something fails, but the failure message has a
-- line number attached.
--
-- Usage:
--
--     countingTests "foo" $ \numberedTest -> do
--       numberedTest someTest
--       numberedTest someTest
--
-- is equivalent to
--
--     describe "foo" $ do
--       it "test 1" someTest
--       it "test 2" someTest
countingTests
  :: Hspec.Example a
  => String
  -> ((a -> Hspec.SpecWith (Hspec.Arg a)) -> Hspec.SpecWith a2)
  -> Hspec.SpecWith a2
countingTests desc withNumberedTest = describe desc $ do
  counter <- Hspec.Spec.runIO $ IORef.newIORef (0 :: Int)
  let numberedTest test = do
        n <- Hspec.Spec.runIO $ IORef.atomicModifyIORef' counter $ \n ->
          (n + 1, n)
        Hspec.Spec.fromSpecList [Hspec.Spec.specItem ("test " <> show n) test]

  withNumberedTest numberedTest
