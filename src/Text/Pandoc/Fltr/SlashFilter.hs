{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter that adds a soft word break (<wbr>) after '/'
module Text.Pandoc.Fltr.SlashFilter (slashFilter) where

import Data.List.Split          (dropBlanks, onSublist, split)
import Text.Pandoc.Definition
import Text.Pandoc.Filter.Utils
import Text.Pandoc.Utils

-- | A helper function for slashFilter, which replaces / with /<wbr> in a
-- string
appendBreak :: String -> [Inline] -> [Inline]
appendBreak "/" acc = Str "/" : RawInline (Format "html") "<wbr>" : acc
appendBreak x   acc = Str (fromString x) : acc

-- | Add a soft word break (<wbr>) after '/'
slashFilter' :: Inline -> [Inline]
slashFilter' (Str str) =
  foldr appendBreak [] $
    split (dropBlanks $ onSublist "/") $ toString str
slashFilter' x = [x]

slashFilter :: PandocFilter
slashFilter = mkFilter slashFilter'
