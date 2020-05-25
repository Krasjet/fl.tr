{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | SVG Renderer for TeX strings inside a markdown file
module Text.Pandoc.Fltr.LaTeX.Renderer (
  mkTeXDocument,
  mkMathDocument,
  compileSVG
) where

import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Fltr.LaTeX.EnvOpts

import qualified Control.Exception as E
import qualified Data.Text.IO      as TIO

import Control.DeepSeq            (NFData (..))
import Control.Monad              (when)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE,
                                   withExceptT)
import Data.Text                  (Text)
import Libkst.Hash
import Libkst.IO
import System.Directory           (removeFile)
import System.Exit                (ExitCode (..))
import System.FilePath            ((<.>), (</>))
import Text.Pandoc.Definition     (MathType (..))
import Text.RawString.QQ

-- | Make latex document for options
mkTeXDocument
  :: Preamble    -- ^ environment and preamble
  -> TeXString   -- ^ tex string to be rendered
  -> TeXDocument -- ^ output TeX document
mkTeXDocument preamble texString =
  [r|\nonstopmode
\documentclass[12pt]{article}
\usepackage[active,tightpage]{preview}
\usepackage{amsmath}
\usepackage{xcolor}
\renewcommand{\rmdefault}{zpltlf}
\usepackage{newpxmath}
\usepackage[scr=rsfso, cal=cm, bb=ams, frak=pxtx]{mathalfa}|]
  <> preamble <>
  [r|\begin{document}
\begin{preview}|]
  <> texString <>
  [r|\end{preview}
\end{document}|]

-- | Make math latex document for options
mkMathDocument
  :: MathType     -- ^ environment and preamble
  -> TeXString    -- ^ tex string to be rendered
  -> TeXDocument  -- ^ output TeX document
mkMathDocument InlineMath texStr = mkTeXDocument mathEnv
  [r|\begin{math}|]
  <> texStr <>
  [r|\end{math}|]
mkMathDocument DisplayMath texStr = mkTeXDocument mathEnv
  [r|\begin{displaymath}|]
  <> texStr <>
  [r|\end{displaymath}|]

-- * More utility functions specific to the renderer

-- | Perform IO action and convert any IOException to our custom exception.
io :: NFData a
   => IO a                     -- ^ IO action
   -> ExceptT RenderError IO a
io = withExceptT IOException . tryIODeep

-- | Perform action on the left. If failed, run action on the right and handle
-- any exceptions.
orElse
  :: IO a           -- ^ IO action that might fail.
  -> ExceptT e IO a -- ^ If action failed, perform this instead
  -> ExceptT e IO a
lft `orElse` rgt = ExceptT $ fmap Right lft `E.catch` handler rgt
  where
    handler
      :: ExceptT e IO a -- ^ action to run instead
      -> E.IOException  -- ^ exception ignored
      -> IO (Either e a)
    handler act _ = runExceptT act

cached
  :: FilePath                     -- ^ Cache directory
  -> FilePath                     -- ^ File hash
  -> ExceptT RenderError IO Text  -- ^ Action
  -> ExceptT RenderError IO Text
cached cacheDir hash action = do
  let cachePath = cacheDir </> hash <.> "svg"
  -- only compile a new one if no cache exists
  TIO.readFile cachePath `orElse` do
    result <- action
    io $ writeFileHandleMissing' cachePath result
    return result

-- | Convert a tex document into a SVG image.
compileSVG
  :: LaTeXFilterOptions -- Filter options
  -> TeXDocument        -- The tex string to be compiled
  -> IO (Either RenderError SVG)
compileSVG opts texDoc = runExceptT $ do
  let docHash = hashText' texDoc
  docCacheDir <- io $ getCacheDir opts

  cached docCacheDir docHash $ do
    tmpDir <- io $ getTempDir opts
    -- write to tex file
    io $ writeFileHandleMissing' (tmpDir </> docHash <.> "tex") texDoc

    -- compile latex file
    -- NOTE remember to restrict file access of tex files in texmf.cnf
    -- see https://tex.stackexchange.com/questions/10418
    (exitCode, out, err) <- io $ readProcessWithCWD tmpDir "latex" [docHash <.> "tex"]
    when (exitCode /= ExitSuccess) $ throwE $ LaTeXFailure (out <> "\n" <> err) texDoc

    -- convert to svg file
    (exitCode', out', err') <- io $ readProcessWithCWD tmpDir "dvisvgm" $
      ["-n", "-j", "-Z", "1.28", "-e"] ++ ["-o", docHash <.> "svg", docHash <.> "dvi"]
      -- ^ no font and clipjoin w/ zoom 23/18
    when (exitCode' /= ExitSuccess) $ throwE $ DVISVGMFailure (out' <> "\n" <> err')

    -- read svg file
    svg <- io $ TIO.readFile (tmpDir </> docHash <.> "svg")

    -- sometimes dvisvgm will fail without notice
    -- force removing the rendered image to prevent propagating error
    io $ removeFile (tmpDir </> docHash <.> "svg")

    -- return
    return svg
