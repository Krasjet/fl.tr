{-# LANGUAGE OverloadedStrings #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | For post-processing SVG files
module Text.Pandoc.Fltr.LaTeX.PostProcessors (
  getImgAttr,
  postProcessSVG
) where


import Text.Pandoc.Definition
import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Utils

import qualified Data.Text as T

import Data.Maybe        (fromMaybe)
import Data.Text         (Text)
import Libkst.Text.Parse
import Numeric

-- | We will retreive the baseline from viewbox parameters
viewboxMarker :: Text
viewboxMarker = " viewBox='"

-- | Get the attrbutes for @img@ tag from an SVG image.
getImgAttr
  :: Pixel -- ^ base font size
  -> SVG   -- ^ SVG image
  -> Attr
getImgAttr base = mkAttr . extractSVGInfo . skipTo viewboxMarker
  where
    -- | Helper function for constructing @img@ tag attribute from 'SVGInfo'.
    mkAttr :: SVGInfo -> Attr
    mkAttr info = nullAttr `addKVPair`
      ("style", "vertical-align:" <> showEm (baseline info) <> ";" <>
                "height:" <> showEm (height info) <> ";" <>
                "width:" <> showEm (width info) <> ";"
      )

    -- | Display point in em as a string
    showEm :: Point -> Text
    showEm pt = toText $ showFFloat (Just 6) (pt2em base pt) "em"

-- | Extract attributes from Text
extractSVGInfo :: Text -> SVGInfo
extractSVGInfo sfx = error (toString sfx)

-- | Remove the id in g tag and clean up comments
postProcessSVG :: SVG -> SVG
postProcessSVG xml = preG <> "<g>" <> postG
  where
    svg = skipTo "<svg" xml
    (preG, gTag) = T.breakOn "<g " svg
    postG = fromMaybe gTag $ skipAfter ">" gTag
