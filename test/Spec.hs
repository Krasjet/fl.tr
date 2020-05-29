{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Test.Tasty
import Test.Tasty.Hspec

import Text.Pandoc.Fltr.BreakCodeFilter
import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Fltr.LaTeX.DocumentBuilder
import Text.Pandoc.Fltr.LaTeX.PostProcessors
import Text.Pandoc.Fltr.ParaFilter
import Text.Pandoc.Fltr.LaTeXFilter
import Text.Pandoc.Fltr.Pygments.PostProcessors

import qualified Text.Pandoc as P
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Data.Text           (Text)
import Control.Monad.Trans.Writer
import System.Directory
import System.FilePath
import Text.Pandoc.Builder
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
  describe "extractEnv" $ do
    it "extracts environment correctly" $
      extractEnv aliEnv `shouldBe` Just "align*"
    it "fails on no env " $ do
      extractEnv noEnv `shouldBe` Nothing
      extractEnv failEnv `shouldBe` Nothing

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

-- * SVG Processing

expectedAttr :: Attr
expectedAttr = nullAttr `addKVPair`
  ("style", "vertical-align:-0.273942em;height:1.281888em;width:5.558170em;")

svgSpec :: Spec
svgSpec = parallel $ do
  svg <- runIO $ TIO.readFile $ "test" </> "data" </> "test" <.> "svg"
  expected <- runIO $ TIO.readFile $ "test" </> "data" </> "expectStrip" <.> "svg"
  describe "postProcessSVG" $
    it "strip comments and id attrs" $
      postProcessSVG svg `shouldBe` expected

  describe "getImgAttr" $
    it "obtains attributes correctly" $
      getImgAttr 16 expected `shouldBe` expectedAttr

-- * Preamble filter

preambleTest :: Text
preambleTest = T.strip [r|
Test
====

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.
```preamble
\newcommand*{\N}{\ensuremath{\mathbb{N}}}
\newcommand*{\Z}{\ensuremath{\mathbb{Z}}}
```
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.

```preamble
\DeclareMathOperator{\lcm}{lcm}
\DeclareMathOperator*{\argmax}{arg\,max}
```

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.
```
\newcommand*{\Q}{\ensuremath{\mathbb{Q}}}
```

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.
|]

preambleTest2 :: Text
preambleTest2 = T.strip [r|
Test2
=====

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.
```
\newcommand*{\N}{\ensuremath{\mathbb{N}}}
\newcommand*{\Z}{\ensuremath{\mathbb{Z}}}
```
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.

```
\DeclareMathOperator{\lcm}{lcm}
\DeclareMathOperator*{\argmax}{arg\,max}
```

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.
```
\newcommand*{\Q}{\ensuremath{\mathbb{Q}}}
```

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.
|]

preambleExpect :: Text
preambleExpect = T.strip [r|
\newcommand*{\N}{\ensuremath{\mathbb{N}}}
\newcommand*{\Z}{\ensuremath{\mathbb{Z}}}
\DeclareMathOperator{\lcm}{lcm}
\DeclareMathOperator*{\argmax}{arg\,max}
|]

preambleDocExpect :: Text
preambleDocExpect = T.strip [r|
Test
====

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.

    \newcommand*{\Q}{\ensuremath{\mathbb{Q}}}

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.
|]

preambleDocExpect2 :: Text
preambleDocExpect2 = T.strip [r|
Test2
=====

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.

    \newcommand*{\N}{\ensuremath{\mathbb{N}}}
    \newcommand*{\Z}{\ensuremath{\mathbb{Z}}}

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.

    \DeclareMathOperator{\lcm}{lcm}
    \DeclareMathOperator*{\argmax}{arg\,max}

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.

    \newcommand*{\Q}{\ensuremath{\mathbb{Q}}}

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean semper pharetra augue at suscipit. Curabitur varius velit ut turpis auctor commodo. Donec porta et tortor aliquet tempus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque porttitor sem eu turpis faucibus malesuada. Donec imperdiet eros eu sapien ornare lacinia. Morbi posuere, quam vel condimentum molestie, magna mi auctor est, efficitur pharetra nulla ex eget felis. Fusce quis nisi dui. Sed id ante ipsum. Nulla facilisi. Aliquam euismod neque eget blandit congue.
|]

preambleSpec :: Spec
preambleSpec = parallel $ do
  let f :: PandocFilterM (Writer Text)
      f = mkFilter preambleFilter

      ropts :: P.ReaderOptions
      ropts = def
        { P.readerExtensions = P.pandocExtensions }

      wopts :: P.WriterOptions
      wopts = def
        { P.writerExtensions = P.pandocExtensions
        , P.writerWrapText = P.WrapNone
        }

      pDoc :: Pandoc
      pDoc = case P.runPure $ P.readMarkdown ropts preambleTest of
        Left e -> error $ "check preamble doc" <> show e
        Right d -> d

      pDoc2 :: Pandoc
      pDoc2 = case P.runPure $ P.readMarkdown ropts preambleTest2 of
        Left e -> error $ "check preamble doc" <> show e
        Right d -> d


  describe "preamble filter" $ do
    let (pDoc', p) = runWriter (applyFilterM f pDoc)
    let (pDoc2', p2) = runWriter (applyFilterM f pDoc2)
    it "extracts preamble from document" $
      T.strip p `shouldBe` preambleExpect

    it "extracts nothing if document doesn't have preamble" $
      T.strip p2 `shouldBe` ""

    it "clean up left over code block" $ do
      let processed :: Text
          processed = case P.runPure $ P.writeMarkdown wopts pDoc' of
            Left e -> error $ "check preamble doc" <> show e
            Right d -> d
      T.strip processed `shouldBe` preambleDocExpect

    it "doesn't touch anything if document doesn't have preamble" $ do
      let processed :: Text
          processed = case P.runPure $ P.writeMarkdown wopts pDoc2' of
            Left e -> error $ "check preamble doc" <> show e
            Right d -> d
      T.strip processed `shouldBe` preambleDocExpect2

-- * Pygments

pygId :: Text
pygId = "testid"

pygCls :: [Text]
pygCls = ["sourceCode", "test", "class"]

pygKVPairs :: [(Text,Text)]
pygKVPairs = [("style", "width: 500px;"), ("title", "C++")]

pygmentsSpec :: Spec
pygmentsSpec = parallel $ do
  input <- runIO $ TIO.readFile $ "test" </> "data" </> "pyginput" <.> "html"
  expected <- runIO $ TIO.readFile $ "test" </> "data" </> "expectPyg" <.> "html"
  describe "addAttrs" $ do
    it "add corresponding attributes to highlighted code" $
      addAttrs pygId pygCls pygKVPairs input `shouldBe` expected

    it "doesn't touch anything if no attributes provided" $
      addAttrs "" [] [] input `shouldBe` input

-- * Para filter

paraDoc :: Pandoc
paraDoc = doc $
  para (str "para") <>
  para (str "para2") <>
  plain (str "plain1") <>
  plain (code "code" <> str "plain2") <>
  bulletList [plain "plain-bullet", para "para-bullet"]

expectParaDoc :: Pandoc
expectParaDoc = doc $
  para (str "para") <>
  para (str "para2") <>
  para (str "plain1") <>
  para (code "code" <> str "plain2") <>
  bulletList [plain "plain-bullet", para "para-bullet"]

paraSpec :: Spec
paraSpec = parallel $
  describe "paraFilter" $
    it "only removes top level plain block" $
      applyFilter paraFilter paraDoc `shouldBe` expectParaDoc

main :: IO ()
main = do
  testBreakCode <- testSpec "Break code filter" breakCodeSpec
  testEnvParse <- testSpec "Env parser" envParserSpec
  testOpts <- testSpec "LaTeX filter options" optsSpec
  testSVGProc <- testSpec "SVG Processing" svgSpec
  testPreamble <- testSpec "Preamble filter" preambleSpec
  testPygments <- testSpec "Pygment postprocessor" pygmentsSpec
  testPara <- testSpec "Paragraph wrapper" paraSpec
  defaultMain $ testGroup "Tests"
    [ testBreakCode
    , testEnvParse
    , testOpts
    , testSVGProc
    , testPreamble
    , testPygments
    , testPara
    ]
