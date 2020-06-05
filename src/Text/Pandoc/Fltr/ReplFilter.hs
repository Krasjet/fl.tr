{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Jupyter-like input output repl
module Text.Pandoc.Fltr.ReplFilter (
  replFilter
) where

import qualified Data.Text as T

import Data.IORef
import Data.Text                          (Text)
import Text.Pandoc.Fltr.Pygments.Renderer

import Text.Pandoc.Definition
import Text.Pandoc.Utils

-- | Add repl with highlighting, the version in renderer is not general enough
-- for this.
addRepl
  :: FilePath                   -- ^ Cache directory
  -> IORef Int                  -- ^ name store
  -> Block -> IO Block
addRepl cacheDir cp b@(CodeBlock attr@(_, cls, _) code) = do
  res <- highlightCodeRaw cacheDir attr code
  case res of
    Nothing   -> return b
    Just html -> RawBlock "html" <$> addPrompt html
  where
    -- | Add prompt to processed html
    addPrompt
      :: Text    -- ^ html
      -> IO Text -- ^ processed html
    addPrompt html = go $ T.breakOn "<pre>" html
      where
        go (pfx, sfx)
          | "codein" `elem` cls = do
            -- new prompt
            modifyIORef' cp (+1)
            curr <- readIORef cp
            return $ pfx <> "<div class=\"prompt-in\">In ["<> toText (show curr) <>"]</div>" <> sfx
          | "codeout" `elem` cls = do
            -- output of old prompt
            curr <- readIORef cp
            return $ pfx <> "<div class=\"prompt-out\">Out ["<> toText (show curr) <>"]</div>" <> sfx
          | otherwise = return $ pfx <> sfx
addRepl _ _ x = return x

replFilter'
  :: FilePath
  -> Pandoc
  -> IO Pandoc
replFilter' cache pandoc = do
  currPrompt <- newIORef 0
  convertFilterM (addRepl cache currPrompt) pandoc

-- | Apply before pygments filter
replFilter
  :: FilePath         -- ^ Cache directory
  -> PandocFilterM IO
replFilter cache = mkFilter $ replFilter' cache
