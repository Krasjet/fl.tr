{-# LANGUAGE OverloadedStrings #-}

-- | A pandoc filter to transform capital letters into small caps, and
-- reverting smallcaps to capital letters.
module Text.Pandoc.Fltr.SmcpFilter (
  smcpFilter,
  smcpFilterLegacy
) where

import Text.Pandoc.Filter.Utils

import qualified Data.Set  as Set
import qualified Data.Text as T

import Data.Char              (isDigit, isUpper)
import Data.Map               (update)
import Data.Set               (Set)
import Data.Text              (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Utils

smcpChars :: Set Char
smcpChars = Set.fromList ['#', '%']

c2scChars :: Set Char
c2scChars = Set.fromList ['&', '[', ']', '(', ')', '{', '}']

-- | Check if a character should be smcp
isSmcp :: Char -> Bool
isSmcp c = isUpper c || c `Set.member` smcpChars

-- | Check if a character should be c2sc
isc2sc :: Char -> Bool
isc2sc c = c `Set.member` c2scChars

-- | Check if a character should be capital
isCap :: Char -> Bool
isCap = isDigit

-- | Convert the string to a span if necessary
replaceStr
  :: (Char -> Bool) -- ^ condition
  -> Text           -- ^ class
  -> Text
  -> Inline
replaceStr _ _ "" = Str ""
replaceStr cond cls str
  | cond $ T.head str = Span ("", [cls], []) [Str str]
  | otherwise = Str str

-- | Match character with condition and wrap a span around it
replaceFilter
  :: (Char -> Bool)     -- ^ condition
  -> Text             -- ^ class to be wrapped
  -> Inline -> [Inline] -- ^ filter
replaceFilter cond cls (Str str) =
  map (replaceStr cond cls) $
  -- group the elements to save some space
  T.groupBy (\x y -> cond x == cond y) $ toText str
-- retain everything else
replaceFilter _ _ x = [x]

-- | Convert capital letters to smallcaps
smcpFilterInline' :: Inline -> [Inline]
smcpFilterInline' = replaceFilter isSmcp "smcp"

-- | Convert symbols to smallcaps
c2scFilterInline' :: Inline -> [Inline]
c2scFilterInline' = replaceFilter isc2sc "c2sc"

-- | Convert number to caps
capFilterInline' :: Inline -> [Inline]
capFilterInline' = replaceFilter isCap "cap"

-- | Only the value associated with the keys in this list will be small capped
-- in Meta
metaWhitelist :: [Text]
metaWhitelist = ["title", "pagetitle"]

-- | Convert capital letters to smcp.
smcpFilterLegacy' :: Pandoc -> Pandoc
smcpFilterLegacy' (Pandoc (Meta meta) blocks) = Pandoc (Meta meta') blocks'
  where
    blockFltr :: PartialFilter [Block]
    blockFltr = mkConcatedFilter [c2scFilterInline', smcpFilterInline']

    blocks' = applyFilter blockFltr blocks
    meta' = foldr (update $ Just . getFilter blockFltr) meta metaWhitelist

-- | Previously convert capital letters to smcp, but now used to convert digits
-- back to capital letters.
smcpFilter' :: Pandoc -> Pandoc
smcpFilter' (Pandoc (Meta meta) blocks) = Pandoc (Meta meta') blocks'
  where
    blockFltr :: PartialFilter [Block]
    blockFltr = mkFilter capFilterInline'

    blocks' = applyFilter blockFltr blocks
    meta' = foldr (update $ Just . getFilter blockFltr) meta metaWhitelist

smcpFilter :: PandocFilter
smcpFilter = mkFilter smcpFilter'

smcpFilterLegacy :: PandocFilter
smcpFilterLegacy = mkFilter smcpFilterLegacy'
