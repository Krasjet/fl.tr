{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Test.Tasty
import Test.Tasty.Hspec

import Data.Default
import Data.Text                          (Text)
import System.Directory
import System.FilePath
import Text.Pandoc.Builder
import Text.Pandoc.Fltr.BreakCodeFilter
import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Fltr.LaTeX.EnvOpts
import Text.Pandoc.Utils
import Text.RawString.QQ

-- * Break code filter

breakLimit :: Int
breakLimit = 8

breakDoc1 :: Pandoc
breakDoc1 = doc $
  para (code "veryverylongcode")

breakDoc1E :: Pandoc
breakDoc1E = doc $
  para (codeWith (nullAttr `addClass` "break-all") "veryverylongcode")

breakDoc2 :: Pandoc
breakDoc2 = doc $
  para (code "veryvery")

breakDoc3 :: Pandoc
breakDoc3 = doc $
  para (code "short")

breakCodeSpec :: Spec
breakCodeSpec = parallel $ do
  let f = breakCodeFilter breakLimit
  describe "break code filter" $ do
    it "breaks code > limit" $
      applyFilter f breakDoc1 `shouldBe` breakDoc1E

    it "does not break code = limit" $
      applyFilter f breakDoc2 `shouldBe` breakDoc2

    it "does not break code < limit" $
      applyFilter f breakDoc3 `shouldBe` breakDoc3

-- * Parsing

aliEnv :: Text
aliEnv =
  [r|
  \begin{align*}
  a & = 1
  \end{align*}
  |]

noEnv :: Text
noEnv = [r|$a = 1$|]

failEnv :: Text
failEnv =
  [r|
  \end{align*}
  a & = 1
  \end{align*}
  |]

envParserSpec :: Spec
envParserSpec = parallel $
  describe "findEnv" $ do
    runIO $ putStrLn $ toString aliEnv
    it "finds environment correctly" $
      findEnv aliEnv `shouldBe` Just "align*"
    it "fails on no env " $ do
      findEnv noEnv `shouldBe` Nothing
      findEnv failEnv `shouldBe` Nothing

-- * Test LaTeX options

opts1 :: LaTeXFilterOptions
opts1 = def

opts2 :: LaTeXFilterOptions
opts2 = def { docId = Just "test" }

opts3 :: LaTeXFilterOptions
opts3 = def { cacheDir = Just "cache"
            , tempDir = Just "temp"
            }

opts4 :: LaTeXFilterOptions
opts4 = opts3 { docId = Just "test" }

optsSpec :: Spec
optsSpec = parallel $ do
  sysTmp <- runIO getTemporaryDirectory
  describe "getCacheDir" $ do
    it "returns system directory when Nothing" $ do
      getCacheDir opts1 `shouldReturn` sysTmp </> "kstCache"
      getCacheDir opts2 `shouldReturn` sysTmp </> "kstCache" </> "test"

    it "returns custom directory when Just" $ do
      getCacheDir opts3 `shouldReturn` "cache"
      getCacheDir opts4 `shouldReturn` "cache" </> "test"

  describe "getTempDir" $ do
    it "returns system directory when Nothing" $ do
      getTempDir opts1 `shouldReturn` sysTmp </> "kstTemp"
      getTempDir opts2 `shouldReturn` sysTmp </> "kstTemp" </> "test"

    it "returns custom directory when Just" $ do
      getTempDir opts3 `shouldReturn` "temp"
      getTempDir opts4 `shouldReturn` "temp" </> "test"

main :: IO ()
main = do
  testBreakCode <- testSpec "Break code filter" breakCodeSpec
  testEnvParse <- testSpec "Env parser" envParserSpec
  testOpts <- testSpec "LaTeX filter options" optsSpec
  defaultMain $ testGroup "Tests"
    [ testBreakCode
    , testEnvParse
    , testOpts
    ]
