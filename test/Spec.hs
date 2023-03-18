module Main where

import           Data.Foldable                  ( for_ )
import qualified Data.IORef                    as IORef
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.String                    ( IsString )
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
          , ([".0f"]  , 123.4, "123")
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
          [ (["quotes-always"]   , "foo"    , "\"foo\"")
          , (["quotes-sometimes"], "foo"    , "foo")
          , (["quotes-never"]    , "foo"    , "foo")
          , (["quotes-always"]   , "foo-bar", "\"foo-bar\"")
          , (["quotes-sometimes"], "foo-bar", "foo-bar")
          , (["quotes-never"]    , "foo-bar", "foo-bar")
          , (["quotes-always"]   , "foo bar", "\"foo bar\"")
          , (["quotes-sometimes"], "foo bar", "\"foo bar\"")
          , (["quotes-never"]    , "foo bar", "foo bar")
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

  describe "Bunch of tests" $ do
    -- It's a hassle to come up with descriptive test names, but convenient for
    -- them all to be unique. This gives them numbers. We can't search for the
    -- test name in source if something fails, but the failure message has a
    -- line number attached.
    counter <- Hspec.Spec.runIO $ IORef.newIORef (0 :: Int)
    let numberedTest test = do
          n <- Hspec.Spec.runIO $ IORef.atomicModifyIORef' counter $ \n ->
            (n + 1, n)
          Hspec.Spec.fromSpecList [Hspec.Spec.specItem ("test " <> show n) test]

    numberedTest $ do
      layout 80 (gist [Gist.strConfig @[] "show-first 3"] [(), (), ()])
        `shouldBe` "[(), (), ()]"

    numberedTest $ do
      layout 80 (gist [Gist.strConfig @[] "show-first 3"] [(), (), (), (), ()])
        `shouldBe` "[(), (), (), ...]"

    do
      let arr :: [Double] = [1.1, 1.11, 1.111, 1.1111]

      numberedTest $ do
        layout
            80
            (gist
              [Gist.strConfig @[] "show-first 3", Gist.strConfig @Double ".2f"]
              arr
            )
          `shouldBe` "[1.10, 1.11, 1.11, ...]"

      numberedTest $ do
        layout
            10
            (gist
              [Gist.strConfig @[] "show-first 3", Gist.strConfig @Double ".2f"]
              arr
            )
          `shouldBe` Text.intercalate
                       "\n"
                       ["[ 1.10", ", 1.11", ", 1.11", ", ... ]"]

      numberedTest $ do
        layout
            80
            (gist
              [ Gist.strConfig @Double ".2f"
              , Gist.config @[] (mempty, pure $ Gist.strConfig @Double ".3f")
              ]
              arr
            )
          `shouldBe` "[1.100, 1.110, 1.111, 1.111]"

      numberedTest $ do
        layout
            80
            (gist
              [ Gist.strConfig @Double ".2f"
              , Gist.config @[] (mempty, pure $ Gist.strConfig @Double ".3f")
              ]
              (1.1 :: Double, arr)
            )
          `shouldBe` "(1.10, [1.100, 1.110, 1.111, 1.111])"

    do
      let map_ = Map.fromList [(3.2 :: Double, ["foo" :: String, "foo bar"])]

      numberedTest $ do
        layout 80 (gist [] map_) `shouldBe` "{3.2: [foo, \"foo bar\"]}"

      numberedTest $ do
        layout 5 (gist [] map_) `shouldBe` Text.intercalate
          "\n"
          [ -- comment to force multi-line layout
            "{ 3.2: [ foo"
          , "       , \"foo bar\" ] }"
          ]

      numberedTest $ do
        layout
            80
            (gist
              [ Gist.strConfig @Map "hide-keys"
              , Gist.strConfig @IsString "quotes-never"
              ]
              map_
            )
          `shouldBe` "{_: [foo, foo bar]}"

      numberedTest $ do
        layout
            80
            (gist
              [Gist.strConfig @Map "hide-keys", Gist.strConfig @Map "hide-vals"]
              map_
            )
          `shouldBe` "{_: _}"

    numberedTest $ do
      layout
          80
          ( gist
              [ Gist.config @Map
                  ( mempty
                  , pure $ Gist.strConfig @IsString "quotes-never"
                  , pure $ Gist.strConfig @IsString "quotes-always"
                  )
              ]
          $ Map.fromList [("foo bar" :: String, "baz" :: String)]
          )
        `shouldBe` "{foo bar: \"baz\"}"

    do
      let
        config =
          [Gist.strConfig @Double ".2f", Gist.strConfig @String "quotes-always"]

      numberedTest $ do
        layout 80 (gist config $ Left @Double @String 3.2)
          `shouldBe` "Left 3.20"

      numberedTest $ do
        layout 80 (gist config $ Right @Double @String "foo")
          `shouldBe` "Right \"foo\""

    do
      let config =
            [ Gist.strConfig @Double ".2f"
            , Gist.config @Either (mempty, pure mempty)
            ]

      numberedTest $ do
        layout 80 (gist config $ Left @Double @Double 3.2)
          `shouldBe` "Left 3.20"

      numberedTest $ do
        layout 80 (gist config $ Right @Double @Double 3.2)
          `shouldBe` "Right 3.2"

    numberedTest $ do
      layout 80 (gist [] (Left (Left ()) :: Either (Either () ()) ()))
          `shouldBe` "Left (Left ())"
