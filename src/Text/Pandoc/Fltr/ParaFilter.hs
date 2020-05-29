-- | Pandoc has some issue parsing paragraphs containing LaTeX code. This
-- filter will make sure there is no 'Plain' block in the top level
module Text.Pandoc.Fltr.ParaFilter (
  paraFilter,
) where

import Text.Pandoc.Filter.Utils

import Text.Pandoc.Definition

wrapPlain :: Block -> Block
wrapPlain (Plain ils) = Para ils
wrapPlain x           = x

-- | Always apply this filter first!!!
paraFilter' :: Pandoc -> Pandoc
paraFilter' (Pandoc meta blks) = Pandoc meta $ map wrapPlain blks

paraFilter :: PandocFilter
paraFilter = mkFilter paraFilter'
