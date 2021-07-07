{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Jupyter-like input output repl
module Text.Pandoc.Fltr.AdmonitionFilter (
  admonitionFilter
) where

import Text.Pandoc.Utils
import Text.Pandoc.Definition

admFilter :: Block -> Block
admFilter x@(Div attr@(_, cls, _) blks)
  | "note" `elem` cls =
      Div attr $ RawBlock "html" "<div class=\"adm-title\">Note</div>" : blks
  | otherwise = x
admFilter x           = x

admonitionFilter :: PandocFilter
admonitionFilter = mkFilter admFilter
