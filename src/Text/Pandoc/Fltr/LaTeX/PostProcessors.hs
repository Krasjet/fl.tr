{-# LANGUAGE OverloadedStrings #-}

-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | For post-processing SVG files
module Text.Pandoc.Fltr.LaTeX.PostProcessors (
  getImgAttr,
  postProcessSVG
) where

import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Definition

import Libkst.Text.Parse

import Data.Text           (Text)

-- | We will retreive the baseline from viewbox parameters
viewboxMarker :: Text
viewboxMarker = " viewBox='"

-- | Get the attrbutes
getImgAttr :: SVG -> Attr
getImgAttr = mkAttr . extractSVGInfo . skipTo viewboxMarker

mkAttr :: SVGInfo -> Attr
mkAttr = error "TODO"


-- -- | attributes
-- baseAdjust :: [(Text, Text)]
-- baseAdjust = [("style", "vertical-align:" <> toText (showFFloat (Just 6) baseline "") <> "pt")]

-- | Extract attributes from Text
extractSVGInfo :: Text -> SVGInfo
extractSVGInfo sfx = error "TODO"

-- | Remove the id in g tag and clean up comments
postProcessSVG :: SVG -> SVG
postProcessSVG xml = error "TODO"
  -- preG <> "<g>" <> postG
  --   where
  --     (_, svg)   = spanL "<svg" xml
  --     (preG, gTag) = spanL "<g " svg
  --     (_, postG) = spanR '>' gTag
