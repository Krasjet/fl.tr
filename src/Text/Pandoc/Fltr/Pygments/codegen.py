#!/usr/bin/env python
from _mapping import LEXERS
from typing import List

template: str = r"""{-# LANGUAGE OverloadedStrings #-}

-- | Lexer mappings in Pygments filter
--
-- **Generated code, DO NOT modify**.
module Text.Pandoc.Fltr.Pygments.Lexers (
  langMapping
) where

import qualified Data.Map as Map

import Data.Map  (Map)
import Data.Text (Text)

-- | Mapping from aliases to language name
langMapping :: Map Text Text
langMapping = Map.fromList
"""

langMaps: List[str] = []

for _, lang, aliases, *_ in LEXERS.values():
    for alias in aliases:
        langMaps.append(f'("{alias}", "{lang}")')

template += "  [ " + "\n  , ".join(langMaps) + "\n  ]"
print(template)
