{-# LANGUAGE OverloadedStrings #-}

-- | Post process html generated by pygments
module Text.Pandoc.Fltr.Pygments.PostProcessors (
  addAttrs
) where

import qualified Data.Text as T

import Data.Text         (Text)
import Chirp.Text.Parse

-- | Add html
addAttrs
  :: Text          -- ^ Id
  -> [Text]        -- ^ Classes
  -> [(Text,Text)] -- ^ key-value pairs
  -> Text          -- ^ Html string
  -> Text          -- ^ processes Html string
addAttrs id' cls pairs html = maybe html (addAttrsToTag id' cls pairs "div" <>) $ skipAfter "<div>" html

addAttrsToTag
  :: Text          -- ^ Id
  -> [Text]        -- ^ Classes
  -> [(Text,Text)] -- ^ key-value pairs
  -> Text          -- ^ Tag
  -> Text          -- ^ Processed tag
addAttrsToTag id' cls pairs tag = "<" <> tag <> idStr <> clsStr <> pairsStr <> ">"
  where
    idStr
      | T.null id' = ""
      | otherwise = " id=\"" <> id' <> "\""
    clsStr
      | null cls = ""
      | otherwise = " class=\"" <> T.intercalate " " cls <> "\""
    pairsStr
      | null pairs = ""
      | otherwise = T.concat $ map (\(k,v) -> " " <> k <> "=\"" <> v <> "\"") pairs
