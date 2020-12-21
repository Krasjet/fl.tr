{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-
 - Adapted from an early version of [1] with support for responsive SVG,
 - custom preambles, environment parsing, and environment-specific preambles.
 - [1]: https://github.com/phadej/latex-svg
 - 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor
 -}

-- | Options for LaTeX environments
module Text.Pandoc.Fltr.LaTeX.DocumentBuilder (
  -- * Definitions
  LaTeXEnv,
  Preamble,
  -- * Environments
  extractEnv,
  -- * Preambles
  preambleTable,
  lookupPreamble,
  defPreamble,
  tikzPreamble,
  -- * Document builders
  mkTeXDocument,
  mkMathDocument,
) where

import Text.Pandoc.Fltr.LaTeX.Definitions

import qualified Data.Map as Map

import Data.Map               (Map)
import Data.Maybe             (fromMaybe)
import Chirp.Text.Parse
import Text.Pandoc.Definition (MathType (..))
import Text.RawString.QQ

-- | Extract the LaTeX environment from a string
extractEnv
  :: TeXString      -- ^ LaTeX code snippet
  -> Maybe LaTeXEnv -- ^ Environment of the code
extractEnv = betweenC' "\\begin{" '}'

-- * Some default preambles

-- | A lookup table for environments. Maybe a case of statement should suffice.
preambleTable :: Map LaTeXEnv Preamble
preambleTable = Map.fromList
  [ ("math", defPreamble)
  , ("display", defPreamble)
  , ("tikzpicture", tikzPreamble)
  ]

-- | Lookup preamble from LaTeX environment
lookupPreamble
  :: Maybe LaTeXEnv -- ^ LaTeX environment
  -> Preamble       -- ^ Corresponding preamble
lookupPreamble = fromMaybe defPreamble . (flip Map.lookup preambleTable =<<)

-- | Default preamble
defPreamble :: Preamble
defPreamble = [r|\usepackage{amssymb}
\usepackage{amsfonts}
\renewcommand*{\vec}[1]{\mathbf{#1}}
\newcommand*{\im}{\ensuremath{{\mkern0.7mu\mathrm{i}\mkern1mu}}}
\newcommand*{\E}{\ensuremath{{\mkern1mu\mathrm{e}\mkern1mu}}}
\newcommand*{\dif}{\mathop{}\!\mathrm{d}}
\newcommand*{\N}{\ensuremath{\mathbb{N}}}
\newcommand*{\Z}{\ensuremath{\mathbb{Z}}}
\newcommand*{\Q}{\ensuremath{\mathbb{Q}}}
\newcommand*{\R}{\ensuremath{\mathbb{R}}}
\newcommand*{\Cplx}{\ensuremath{\mathbb{C}}}
\newcommand*{\Trans}{\ensuremath{{\mkern-1.5mu\mathsf{T}}}}
\DeclareMathOperator{\lcm}{lcm}
\DeclareMathOperator*{\argmax}{arg\,max}
\DeclareMathOperator*{\argmin}{arg\,min}
\renewcommand*{\star}{{\mathbin{\text{\usefont{OML}{cmm}{m}{it}\symbol{"3F}}}}}
\renewcommand*{\ast}{{\mathbin{\text{\usefont{OMS}{cmsy}{m}{n}\symbol{"03}}}}}
\DeclareFontEncoding{FML}{}{}
\DeclareFontSubstitution{FML}{futm}{m}{it}
\renewcommand*{\theta}{{\mathord{\text{\usefont{FML}{futmi}{m}{it}\symbol{"12}}}}}
\DeclareFontFamily{U}{wncy}{}
\DeclareFontShape{U}{wncy}{m}{n}{<->wncyr10}{}
\newcommand*{\Shah}{{\mathord{\text{\usefont{U}{wncy}{m}{n}\symbol{"58}}}}}
\makeatletter
\renewenvironment{bmatrix}{\left[\mkern3mu\env@matrix}{\endmatrix\mkern3mu\right]}
\renewenvironment{vmatrix}{\left\lvert\mkern5mu\env@matrix}{\endmatrix\mkern5mu\right\rvert}
\makeatother|]

tikzPreamble :: Preamble
tikzPreamble = defPreamble <> [r|\usepackage{tikz}
\usepackage{pgfplots}
\usetikzlibrary{arrows,calc,patterns,angles,quotes,3d,arrows.meta,positioning}
\pgfplotsset{compat=1.16}|]

-- * Document builder

-- | Make latex document for options
mkTeXDocument
  :: Preamble    -- ^ Preamble
  -> TeXString   -- ^ TeX string to be rendered
  -> TeXDocument -- ^ Output TeX document
mkTeXDocument preamble texString =
  [r|\nonstopmode
\documentclass[12pt]{article}
\usepackage[active,tightpage]{preview}
\usepackage{amsmath}
\usepackage{xcolor}
\renewcommand{\rmdefault}{zpltlf}
\usepackage{newpxmath}
\usepackage[scr=rsfso, cal=cm, bb=ams, frak=pxtx]{mathalfa}
|]
  <> preamble <>
  [r|
\begin{document}
\begin{preview}
|]
  <> texString <>
  [r|
\end{preview}
\end{document}|]

-- | Make math latex document for options
mkMathDocument
  :: MathType     -- ^ inline or display math
  -> Preamble     -- ^ extra preambles
  -> TeXString    -- ^ tex string to be rendered
  -> TeXDocument  -- ^ output TeX document
mkMathDocument InlineMath ext texStr = mkTeXDocument (defPreamble <> ext) $
    [r|\begin{math}|]
    <> texStr <>
    [r|\end{math}|]
mkMathDocument DisplayMath ext texStr = mkTeXDocument (defPreamble <> ext) $
    [r|\begin{displaymath}|]
    <> texStr <>
    [r|\end{displaymath}|]
