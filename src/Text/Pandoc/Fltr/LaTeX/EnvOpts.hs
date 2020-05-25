{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | Options for LaTeX environments
module Text.Pandoc.Fltr.LaTeX.EnvOpts (
  LaTeXEnv,
  Preamble,
  findEnv,
  envTable,
  lookupPreamble,
  mathEnv,
  tikzEnv
) where

import Text.Pandoc.Fltr.LaTeX.Definitions
import Libkst.Text.Parse

import qualified Data.Map as Map

import Data.Map          (Map)
import Data.Maybe        (fromMaybe)
import Data.Text         (Text)
import Text.RawString.QQ

type LaTeXEnv = Text
type Preamble = Text

-- | Search the environment in a tex string
findEnv
  :: TeXString      -- ^ LaTeX code snippet
  -> Maybe LaTeXEnv -- ^ Environment of the code
findEnv = betweenC' "\\begin{" '}'

-- | A lookup table for environments
-- maybe a case of statement should suffice
envTable :: Map LaTeXEnv Preamble
envTable = Map.fromList
  [ ("math", mathEnv)
  , ("display", mathEnv)
  , ("tikzpicture", tikzEnv)
  ]

-- | Lookup preamble from LaTeX environment
lookupPreamble :: Maybe LaTeXEnv -> Preamble
lookupPreamble = fromMaybe mathEnv . (flip Map.lookup envTable =<<)

-- * Environments

mathEnv :: LaTeXEnv
mathEnv = [r|
\usepackage{amssymb}
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
\makeatother
|]

tikzEnv :: LaTeXEnv
tikzEnv = mathEnv <> [r|
\usepackage{tikz}
\usepackage{pgfplots}
\usetikzlibrary{arrows,calc,patterns,angles,quotes,3d,arrows.meta,positioning}
\pgfplotsset{compat=1.16}
|]
