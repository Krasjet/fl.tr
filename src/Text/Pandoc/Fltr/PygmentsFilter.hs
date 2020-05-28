{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Use Pygments to highlight code instead
module Text.Pandoc.Fltr.PygmentsFilter (
  pygmentsFilter
) where

import Control.Monad.Trans.Except               (runExceptT)
import Data.Map                                 ((!?))
import Data.Maybe                               (mapMaybe)
import Data.Text                                (Text)
import Libkst.IO                                (tryIODeep)
import System.Process                           (readProcess)
import Text.Pandoc.Definition
import Text.Pandoc.Fltr.Pygments.Lexers
import Text.Pandoc.Fltr.Pygments.PostProcessors
import Text.Pandoc.Utils

-- | Highlight code block using pygments
highlightCode :: Block -> IO Block
highlightCode b@(CodeBlock (id', cls, kvpairs) code) =
  go $ mapMaybe (\c -> (c,) <$> (langMapping !? c)) cls -- check if language is valid
  where
    go :: [(Text,Text)] -> IO Block
    go langMaps
      | null langMaps = return b
      | otherwise = do
        let hlLines = maybe "" (\c -> ",hl_lines=\"" <> c <> "\"") $ lookup "hl_lines" kvpairs
        res <- runExceptT $ tryIODeep $
                  readProcess "pygmentize"
                    [ "-l", getAlias langMaps
                    , "-O", "wrapcode,cssclass=" <> toString hlLines
                    , "-f", "html"
                    ] $ toString code
        case res of
          Left _ -> return b -- keep block if error
          Right html -> return $ RawBlock "html" $!
            addAttrs id' ("sourceCode":cls) kvpairs $ fromString html

    getAlias :: [(Text,Text)] -> String
    getAlias = toString . fst. head
highlightCode x = return x

pygmentsFilter :: PandocFilterM IO
pygmentsFilter = mkFilter highlightCode
