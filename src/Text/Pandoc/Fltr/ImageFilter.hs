{-# LANGUAGE OverloadedStrings #-}

-- | Add tags to images
module Text.Pandoc.Fltr.ImageFilter (
  imageFilter,
) where

import qualified Data.Text as T

import Data.List              (delete)
import Data.Maybe             (isJust)
import Data.Text              (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Utils

-- | Build attributes for images
imageAttr :: [Text] -> Attr
imageAttr cls = nullAttr `addClasses` ("figure":cls)

-- | Wraps images in a top level div, plus some extra functionalities to add a
-- links to an image
imageFilter' :: Block -> Block
imageFilter' blk@(Para img@[Image (_,cls,_) _ tgt@(_, titl)])
  | isJust (T.stripPrefix "fig:" titl) = blk
  | "linked" `elem` cls = Div (imageAttr cls') [Para [Link (nullAttr `addClasses` cls') img tgt]]
  | otherwise = Div (imageAttr cls) [blk]
    where cls' = delete "linked" cls
imageFilter' blk@(Para [Link (_,clsL,_) [Image (_,clsImg,_) _ _] _]) =
  Div (imageAttr $ clsL <> clsImg) [blk]
imageFilter' x = x

imageFilter :: PandocFilter
imageFilter = mkFilter imageFilter'
