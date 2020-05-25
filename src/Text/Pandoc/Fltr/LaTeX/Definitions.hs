-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | Definitions for the LaTeX filter
module Text.Pandoc.Fltr.LaTeX.Definitions (
  -- * Definitions
  TeXString,
  TeXDocument,
  BaseLine,
  SVG,
  -- * Options
  LaTeXFilterOptions(..),
  getCacheDir,
  getTempDir,
  -- * Errors
  RenderError(..),
) where

import qualified Control.Exception as E

import Data.Default
import Data.Maybe       (fromMaybe)
import Data.Text        (Text)
import Libkst.Html      (SVG)
import System.Directory (getTemporaryDirectory)
import System.FilePath  ((</>))

-- | A raw TeX source code snippet. Can be either a formula or environment.
type TeXString = Text

-- | A TeX document
type TeXDocument = Text

-- | Number of points (@pt@) from the bottom of the image to the typesetting
-- baseline (min-y).
type BaseLine = Double

data LaTeXFilterOptions
   = LaTeXFilterOptions
   { fontScaling :: !Double
     -- ^ Scaling of font
   , cacheDir    :: !(Maybe FilePath)
     -- ^ Cache directory path. 'Nothing' implies use system temp dir.
   , tempDir     :: !(Maybe FilePath)
     -- ^ Temporary directory path used for compling. 'Nothing' implies use system temp dir.
   , docId       :: !(Maybe FilePath)
     -- ^ Temp files will be organized by docId. 'Nothing' implies temp files will be written to root directory of @tempDir@.
   }
  deriving (Show, Eq, Read)

instance Default LaTeXFilterOptions where
  def = LaTeXFilterOptions
    { fontScaling = 1.0
    , cacheDir = Nothing
    , tempDir = Nothing
    , docId = Nothing
    }

-- | Get cache directory from options
getCacheDir
  :: LaTeXFilterOptions
  -> IO FilePath
getCacheDir opts = do
  let dId = fromMaybe "" $ docId opts
  case cacheDir opts of
    Just path -> return $ path </> dId
    Nothing -> do
      tmp <- getTemporaryDirectory
      return $ tmp </> "kstCache" </> dId

-- | Get temp directory from options
getTempDir
  :: LaTeXFilterOptions
  -> IO FilePath
getTempDir opts = do
  let dId = fromMaybe "" $ docId opts
  case tempDir opts of
    Just path -> return $ path </> dId
    Nothing -> do
      tmp <- getTemporaryDirectory
      return $ tmp </> "kstTemp" </> dId

-- | This type contains all possible errors than can happen while rendering an
-- equation. It includes all IO errors that can happen as well as more specific
-- errors.
data RenderError
  = LaTeXFailure String TeXDocument -- ^ @latex@ returned a nonzero error code
  | DVISVGMFailure String           -- ^ @dvisvgm@ returned a nonzero error code
  | IOException E.IOException       -- ^ An 'IOException' occurred while managing the temporary files used to convert the equation
  deriving (Show, Eq)
