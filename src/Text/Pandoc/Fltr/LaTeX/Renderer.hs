{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | SVG Renderer for TeX strings inside a markdown file
module Text.Pandoc.Fltr.LaTeX.Renderer (
  compileSVG
) where

import Text.Pandoc.Fltr.LaTeX.Definitions

import qualified Data.Text.IO as TIO

import Control.DeepSeq            (NFData)
import Control.Monad              (when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Data.Text                  (Text)
import Libkst.Hash
import Libkst.IO
import System.Directory           (removeFile)
import System.Exit                (ExitCode (..))
import System.FilePath            ((<.>), (</>))

-- | Perform IO action and convert any IOException to our custom exception.
io :: NFData a
   => IO a                     -- ^ IO action
   -> ExceptT RenderError IO a
io = withExceptT IOException . tryIODeep

cached
  :: FilePath                     -- ^ Cache directory
  -> FilePath                     -- ^ File hash
  -> ExceptT RenderError IO Text  -- ^ Action
  -> ExceptT RenderError IO Text
cached dir hash action = do
  let cachePath = dir </> hash <.> "svg"
  -- only compile a new one if no cache exists
  TIO.readFile cachePath `orElseIO` do
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
      ["-n", "-j", "-Z", show $ imageScaling opts, "-e"] ++ ["-o", docHash <.> "svg", docHash <.> "dvi"]
      -- ^ no font and clipjoin w/ zoom 23/18
    when (exitCode' /= ExitSuccess) $ throwE $ DVISVGMFailure (out' <> "\n" <> err')

    -- read svg file
    svg <- io $ TIO.readFile (tmpDir </> docHash <.> "svg")

    -- sometimes dvisvgm will fail without notice
    -- force removing the rendered image to prevent propagating error
    io $ removeFile (tmpDir </> docHash <.> "svg")

    -- return
    return svg
