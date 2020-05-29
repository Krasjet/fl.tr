{-# LANGUAGE OverloadedStrings #-}

-- | Add tags to images
module Text.Pandoc.Fltr.ImageFilter (
  imageFilter,
) where

import qualified Data.Text as T

import Data.Maybe (isJust)
import Text.Pandoc.Definition
import Text.Pandoc.Utils

-- | Attributes for images
imageAttr :: Attr
imageAttr = nullAttr `addClass` "figure"

-- | Wraps images in a top level div, plus some extra functionalities to add a
-- links to an image
imageFilter' :: Block -> Block
imageFilter' blk@(Para img@[Image (_,cls,_) _ tgt@(_, titl)])
  | isJust (T.stripPrefix "fig:" titl) = blk
  | "linked" `elem` cls = Div imageAttr [Para [Link (nullAttr `addClasses` cls) img tgt]]
  | otherwise = Div imageAttr [blk]
imageFilter' blk@(Para [Link _ [Image{}] _]) =
  Div imageAttr [blk]
imageFilter' x = x

imageFilter :: PandocFilter
imageFilter = mkFilter imageFilter'
