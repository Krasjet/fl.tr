{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | SVG Renderer for TeX strings inside a markdown file
module Text.Pandoc.Fltr.LaTeX.Renderer (mkTeXDoc, mkMathTeXDoc, compileSVG) where

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
mkTeXDoc
  :: Preamble    -- ^ environment and preamble
  -> TeXString   -- ^ tex string to be rendered
  -> TeXDocument -- ^ output TeX document
mkTeXDoc preamble texString =
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
  \end{document}
  |]

-- | Make math latex document for options
mkMathTeXDoc
  :: MathType     -- ^ environment and preamble
  -> TeXString    -- ^ tex string to be rendered
  -> TeXDocument  -- ^ output TeX document
mkMathTeXDoc InlineMath texStr = mkTeXDoc mathEnv
  [r|\begin{math}|]
  <> texStr <>
  [r|\end{math}|]
mkMathTeXDoc DisplayMath texStr = mkTeXDoc mathEnv
  [r|\begin{displaymath}|]
  <> texStr <>
  [r|\end{displaymath}|]

-- | The temp directory for compiling tex file
tmpDir :: String
tmpDir = ".tmp"

-- | The basename of the temporary file for compiling tex file
tmpFile :: String
tmpFile = "compiling"

-- * More utility functions specific to the renderer

io :: NFData a => IO a -> ExceptT RenderError IO a
io = withExceptT IOException . tryIODeep

handler :: ExceptT e IO a -> E.IOException -> IO (Either e a)
handler rgt _ = runExceptT rgt

orElse :: IO a -> ExceptT e IO a -> ExceptT e IO a
orElse lft rgt = ExceptT $ fmap Right lft `E.catch` handler rgt

cached
  :: String                        -- ^ cache directory
  -> TeXDocument                   -- ^ tex document
  -> ExceptT RenderError IO Text -- ^ action
  -> ExceptT RenderError IO Text
cached cacheDir texDoc action = do
  -- the cache
  let path = cacheDir </> hashText' texDoc <.> "svg"
  -- only compile a new one if no cache exists
  TIO.readFile path `orElse` do
    result <- action
    io $ writeFileHandleMissing' path result
    return result

-- | Convert a tex string into a SVG image.
compileSVG
  :: FilePath          -- cache directory for svg files
  -> TeXDocument        -- the tex string to be compiled
  -> IO (Either RenderError SVG)
compileSVG cacheDir texDoc = runExceptT $
  cached cacheDir texDoc $ do
    -- write to tex file
    io $ writeFileHandleMissing' (tmpDir </> tmpFile <.> "tex") texDoc

    -- compile latex file
    -- NOTE remember to restrict file access of tex files in texmf.cnf
    -- see https://tex.stackexchange.com/questions/10418
    (exitCode, out, err) <- io $ readProcessWithCWD tmpDir "latex" [tmpFile <.> "tex"]
    when (exitCode /= ExitSuccess) $ throwE $ LaTeXFailure (out <> "\n" <> err) texDoc

    -- convert to svg file
    (exitCode', out', err') <- io $ readProcessWithCWD tmpDir "dvisvgm" $
      ["-n", "-j", "-Z", "1.28", "-e"] ++ ["-o", tmpFile <.> "svg", tmpFile <.> "dvi"]
      -- ^ no font and clipjoin w/ zoom 23/18
    when (exitCode' /= ExitSuccess) $ throwE $ DVISVGMFailure (out' <> "\n" <> err')

    -- read svg file
    svg <- io $ TIO.readFile (tmpDir </> tmpFile <.> "svg")

    -- sometimes dvisvgm will fail without notice
    -- force removing the rendered image to prevent propagating error
    io $ removeFile (tmpDir </> tmpFile <.> "svg")

    -- return
    return svg
