{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Use Pygments to highlight code instead
module Text.Pandoc.Fltr.PygmentsFilter (
  pygmentsFilter
) where

import Text.Pandoc.Fltr.Pygments.Renderer
import Text.Pandoc.Utils

import Text.Pandoc.Definition

highlightCode
  :: FilePath           -- ^ Cache directory
  -> Block -> IO Block
highlightCode cacheDir b@(CodeBlock attr code) = do
  res <- highlightCodeRaw cacheDir attr code
  case res of
    Nothing   -> return b
    Just html -> return $ RawBlock "html" html
highlightCode _ x = return x

pygmentsFilter
  :: FilePath         -- ^ Cache directory
  -> PandocFilterM IO
pygmentsFilter cache = mkFilter $ highlightCode cache
