{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter that prevents orphaned em dashes.
module Text.Pandoc.Fltr.DashFilter (dashFilter) where

import Text.Pandoc.Definition
import Text.Pandoc.Filter.Utils
import Text.Pandoc.Utils

import Chirp.List (splitWhen)

-- | A helper function for dashFilter that adds nowrap to strings like "str—"
appendNoWrap :: [String] -> [Inline]
appendNoWrap (x : "\8212" : xs) =
  Span ("", ["nowrap"], []) [Str $ fromString (x ++ "\8212")] : appendNoWrap xs
appendNoWrap (x : xs) = Str (toText x) : appendNoWrap xs
appendNoWrap [] = []

-- | Do not break em dashes!
dashFilter' :: Inline -> [Inline]
dashFilter' (Str str) = appendNoWrap $ splitWhen (=='\8212') $ toString str
dashFilter' x = [x]

dashFilter :: PandocFilter
dashFilter = mkFilter dashFilter'
