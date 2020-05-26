{-# LANGUAGE OverloadedStrings #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | For post-processing SVG files
module Text.Pandoc.Fltr.LaTeX.PostProcessors (
  getImgAttr,
  postProcessSVG
) where

import qualified Data.Attoparsec.Text   as A
import qualified Libkst.Text.Attoparsec as A

import Text.Pandoc.Definition
import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Utils

import qualified Data.Text as T

import Control.Monad        (void)
import Data.Attoparsec.Text (Parser)
import Data.Maybe           (fromMaybe)
import Data.Text            (Text)
import Libkst.Text.Parse
import Numeric

-- * Post processing

-- | Remove the id in g tag and clean up comments
postProcessSVG :: SVG -> SVG
postProcessSVG xml = preG <> "<g>" <> postG
  where
    svg = skipTo "<svg" xml
    (preG, gTag) = T.breakOn "<g " svg
    postG = fromMaybe gTag $ skipAfter ">" gTag

-- * Attribute extraction

-- | The beginning of SVG metadata
widthMarker :: Text
widthMarker = " width='"

-- | Get the attrbutes for @img@ tag from an SVG image.
getImgAttr
  :: Pixel -- ^ base font size
  -> SVG   -- ^ SVG image
  -> Attr
getImgAttr base = mkAttr . extractSVGInfo . skipTo widthMarker
  where
    -- | Helper function for constructing @img@ tag attribute from 'SVGInfo'.
    mkAttr :: SVGInfo -> Attr
    mkAttr info = nullAttr `addKVPair`
      ("style", "vertical-align:" <> showEm (baseline info) <>
                "height:" <> showEm (height info)<>
                "width:" <> showEm (width info)
      )

    -- | Display point in em as a string
    showEm :: Point -> Text
    showEm pt = toText $ showFFloat (Just 6) (pt2em base pt) "em;"

-- | Extract attributes from Text
extractSVGInfo :: Text -> SVGInfo
extractSVGInfo svg = case A.parseOnly parseInfo svg of
  Left e     -> error $ show (e, svg)
  Right info -> info

-- * Parsers

parseInfo :: Parser SVGInfo
parseInfo = do
  -- width
  void $ A.string widthMarker
  w <- A.double' <* "pt"
  void $ A.lexeme $ A.string "'"

  -- height
  void $ A.string "height='"
  h <- A.double' <* "pt"
  void $ A.lexeme $ A.string "'"

  -- viewbox
  void $ A.string "viewBox='"
  void      $ A.lexeme A.double'
  minY     <- A.lexeme A.double'
  void      $ A.lexeme A.double'
  viewboxH <- A.double'

  return SVGInfo
    { baseline = minY + viewboxH
    , height = h
    , width = w
    }
