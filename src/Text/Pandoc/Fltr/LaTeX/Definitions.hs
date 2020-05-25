-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | Definitions for the LaTeX filter
module Text.Pandoc.Fltr.LaTeX.Definitions (
  TeXString,
  TeXDocument,
  BaseLine,
  SVG,
  RenderError(..),
) where

import qualified Control.Exception as E

import Data.Text   (Text)
import Libkst.Html (SVG)

-- | A raw TeX source code snippet. Can be either a formula or environment.
type TeXString = Text

-- | A TeX document
type TeXDocument = Text

-- | Number of points (@pt@) from the bottom of the image to the typesetting
-- baseline (min-y).
type BaseLine = Double

-- | This type contains all possible errors than can happen while rendering an
-- equation. It includes all IO errors that can happen as well as more specific
-- errors.
data RenderError
  = LaTeXFailure String TeXDocument -- ^ @latex@ returned a nonzero error code
  | DVISVGMFailure String           -- ^ @dvisvgm@ returned a nonzero error code
  | IOException E.IOException       -- ^ An 'IOException' occurred while managing the temporary files used to convert the equation
  deriving (Show, Eq)
