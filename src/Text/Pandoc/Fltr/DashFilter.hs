{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter that prevents orphaned em dashes.
module Text.Pandoc.Fltr.DashFilter (dashFilter) where

import qualified Data.Text as T

import Data.List.Split          (dropBlanks, onSublist, split)
import Text.Pandoc.Definition
import Text.Pandoc.Filter.Utils
import Text.Pandoc.Utils

-- | The em dash character
emDash :: String
emDash = "\8212"

-- | A helper function for dashFilter that adds nowrap to strings like "str—"
appendNoWrap :: [String] -> [Inline]
appendNoWrap (x : "\8212" : xs) =
  Span ("", ["nowrap"], []) [Str $ fromString (x ++ emDash)] : appendNoWrap xs
appendNoWrap (x : xs) = Str (T.pack x) : appendNoWrap xs
appendNoWrap [] = []

-- | Do not break em dashes!
dashFilter' :: Inline -> [Inline]
dashFilter' (Str str) = appendNoWrap $
  split (dropBlanks $ onSublist emDash) $ toString str
dashFilter' x = [x]

dashFilter :: PandocFilter
dashFilter = mkFilter dashFilter'
