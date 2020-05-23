{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.Hspec

import Text.Pandoc.Builder
import Text.Pandoc.Filter.Utils
import Text.Pandoc.Filter.Utils.AttrBuilder
import Text.Pandoc.Fltr.BreakCodeFilter

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

main :: IO ()
main = do
  testBreakCode <- testSpec "Break code filter" breakCodeSpec
  defaultMain $ testGroup "Tests"
    [ testBreakCode
    ]
