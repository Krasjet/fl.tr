{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Use Pygments to highlight code instead
module Text.Pandoc.Fltr.PygmentsFilter (
  pygmentsFilter
) where

import qualified Data.Text as T

import Data.Map                         ((!?))
import Data.Maybe                       (mapMaybe)
import System.Process (readProcess)
import Data.Text                        (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Fltr.Pygments.Lexers
import Text.Pandoc.Utils

highlightCode :: Block -> IO Block
highlightCode b@(CodeBlock (id', cls, kvpairs) code) =
  go $ mapMaybe (\c -> (c,) <$> (langMapping !? c)) cls
  where
    go :: [(Text,Text)] -> IO Block
    go langMaps
      | null langMaps = return b
      | otherwise = do
        html <- readProcess "pygmentize"
          [ "-l", getAlias langMaps
          , "-O", "wrapcode,cssclass=" <> "sourceCode " <> fromText (T.intercalate " " cls)
          , "-f", "html"
          ] $ toString code
        -- TODO add id and kvpairs to result
        return $ RawBlock "html" $ fromString html

    getAlias :: [(Text,Text)] -> String
    getAlias = toString . fst. head
highlightCode x = return x

pygmentsFilter :: PandocFilterM IO
pygmentsFilter = mkFilter highlightCode
