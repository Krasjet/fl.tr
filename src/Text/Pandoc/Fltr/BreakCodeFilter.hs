{-# LANGUAGE OverloadedStrings #-}

-- | Break inline code longer than a limit.
module Text.Pandoc.Fltr.BreakCodeFilter (
  breakCodeFilter
) where

import qualified Data.Text as T

import Text.Pandoc.Definition
import Text.Pandoc.Filter.Utils
import Text.Pandoc.Filter.Utils.AttrBuilder

-- | Add 'break-all' class to long inline code
breakCode
  :: Int -- ^ limit
  -> (Inline -> Inline)
breakCode limit il@(Code attr code)
  | T.length code > limit = Code (attr `addClass` "break-all") code
  | otherwise = il
breakCode _ x = x

breakCodeFilter
  :: Int
  -> PandocFilter
breakCodeFilter lim = mkFilter $ breakCode lim
