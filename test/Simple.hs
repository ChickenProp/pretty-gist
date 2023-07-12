module Simple
  ( spec
  ) where

import           Control.Lens
import           Data.Bool                      ( bool )
import           Data.Foldable                  ( for_ )
import           Data.Generics.Product          ( field
                                                , field'
                                                , setField
                                                )
import qualified Data.IORef                    as IORef
import qualified Data.Text                     as Text
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

import qualified ChessBoard.Simple             as CB
import qualified Gist.Simple                   as Gist
import qualified Gist.Simple                   as Gist.ConfigList
                                                ( ConfigList(..) )
import qualified Gist.Simple                   as Gist.ConfigMaybe
                                                ( ConfigMaybe(..) )
import           Gist.Simple                    ( Gist(..) )

spec :: Hspec.Spec
spec = do
  let
    layout n = PPText.renderStrict . PP.layoutPretty
      (PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine n 1 })

  let gist80 conf val = layout 80 (gist conf val)
      gistF80 conf val = layout 80 (gistF conf val)
      gist_80 val = layout 80 (gist_ val)

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
      for_ tests $ \(args, input :: Double, expected) -> do
        let desc =
              concat [show args, " / ", show input, " === ", show expected]
        it desc $ QC.property $ \width ->
          layout width (gistF (field @"printfFmt" .~ args) input)
            `shouldBe` expected

    describe "Float" $ do
      for_ tests $ \(args, input :: Float, expected) -> do
        let desc =
              concat [show args, " / ", show input, " === ", show expected]
        it desc $ QC.property $ \width ->
          layout width (gistF (\c -> c { Gist.printfFmt = args }) input)
            `shouldBe` expected

    describe "MyFloat" $ do
      for_ tests $ \(args, input :: Gist.MyFloat, expected) -> do
        let desc =
              concat [show args, " / ", show input, " === ", show expected]
        it desc $ QC.property $ \width ->
          layout width (gistF (field @"printfFmt" .~ args) input)
            `shouldBe` expected

  countingTests "Bunch of tests" $ \numberedTest -> do
    numberedTest $ do
      gistF80 id [(), (), ()] `shouldBe` "[(), (), ()]"

    numberedTest $ do
      gistF80 (field @"showFirst" .~ Just 2) [(), (), ()]
        `shouldBe` "[(), (), ...]"

    -- With duplicate record fields, if we don't want to use lenses, we need a
    -- type signature to disambiguate. But this throws a warning on 9.2 and 9.4.
    numberedTest $ do
      gistF80
          (\c ->
            c { Gist.gistElem = Gist.FnGister (\_ () -> "_") } :: Gist.ConfigList
                ()
          )
          [()]
        `shouldBe` "[_]"

    -- This is another option, no warnings, but requires a new import line.
    numberedTest $ do
      gistF80
          (\c -> c { Gist.ConfigList.gistElem = Gist.FnGister (\_ () -> "_") })
          [()]
        `shouldBe` "[_]"

    -- This one is fine because `showFirst` isn't duplicated (yet).
    numberedTest $ do
      gistF80
          (\c -> c { Gist.showFirst = Just 3
                   , Gist.gistElem  = Gist.FnGister (\_ () -> "_")
                   }
          )
          [()]
        `shouldBe` "[_]"

    numberedTest $ do
      gistF80
          ( (field @"showFirst" .~ Just 2)
          . (field @"gistElem" .~ Gist.FnGister (\_ () -> "_"))
          )
          [(), (), ()]
        `shouldBe` "[_, _, ...]"

    numberedTest $ do
      -- Could probably be improved with some `Gist.withConf` function that
      -- applies a function to a ConfGister.
      gistF80
          ( (field @"showFirst" .~ Just 2)
          . (field @"gistElem" .~ Gist.ConfGister
              (defaultConfig @[()] & field @"showFirst" .~ Just 2)
            )
          )
          [[(), (), ()], [(), (), ()], [(), (), ()]]
        `shouldBe` "[[(), (), ...], [(), (), ...], ...]"

    numberedTest $ do
      gistF80
          ( (field @"showFirst" .~ Just 2)
          . (field @"gistElem" .~ Gist.FnGister (\_ () -> "_"))
          )
          [(), (), ()]
        `shouldBe` "[_, _, ...]"

    numberedTest $ do
      gist80
          ( defaultConfig @[Void]
            -- doesn't work with field in place of field', but does if we move
            -- it after the gistElem update
          & (field' @"showFirst" .~ Just 2)
          & (  field @"gistElem"
            .~ Gist.FnGister (\_ f -> gistF id (f False, f True))
            )
          )
          [bool () ()]
        `shouldBe` "[((), ())]"

  countingTests "chessboard" $ \numberedTest -> do
    numberedTest $ do
      gist_80 (CB.Piece CB.Pawn CB.Black Nothing)
        `shouldBe` "Piece {pieceType = Pawn, owner = Black, lastMoved = _}"

    numberedTest $ do
      layout (20) (gist_ (CB.Piece CB.Pawn CB.Black Nothing))
        `shouldBe` Text.intercalate
                     "\n"
                     [ "Piece { pieceType = Pawn"
                     , "      , owner = Black"
                     , "      , lastMoved = _"
                     , "      }"
                     ]

    numberedTest $ do
      gistF80 (field @"singleChar" .~ True) (CB.Piece CB.Pawn CB.Black Nothing)
        `shouldBe` "p"

    numberedTest $ do
      gist_80 CB.startPos `shouldBe` CB.renderedShort

    numberedTest $ do
      gistF80
          ( setField @"gistBoard"
          $ Gist.defaultConfGisterF
          $ setField @"gistElem"
          $ Gist.defaultConfGisterF
          $ setField @"gistElem"
          $ Gist.defaultConfGisterF
          $ setField @"gistElem"
          $ Gist.defaultConfGisterF
          $ setField @"singleChar" False
          )
          CB.startPos
        `shouldBe` CB.renderedLong

    numberedTest $ do
      gistF80
          (let
             gPiece =
               Gist.defaultConfGisterF $ \c -> c { CB.singleChar = False }
             gMPiece = Gist.defaultConfGisterF
               $ \c -> c { Gist.ConfigMaybe.gistElem = gPiece }
             gLMPiece = Gist.defaultConfGisterF
               $ \c -> c { Gist.ConfigList.gistElem = gMPiece }
             gBoard = Gist.defaultConfGisterF
               $ \c -> c { Gist.ConfigList.gistElem = gLMPiece }
           in
             \c -> c { CB.gistBoard = gBoard }
          )
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
