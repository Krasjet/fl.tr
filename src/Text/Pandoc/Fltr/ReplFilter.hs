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

import Control.Monad                            (when)
import Control.Monad.Trans.Maybe
import System.Process                           (readProcess)
import Text.Pandoc.Definition
import Text.Pandoc.Fltr.Pygments.PostProcessors
import Text.Pandoc.Utils

-- | Add repl with highlighting, the version in renderer is not general enough
-- for this.
-- TODO move back to renderer
addRepl
  :: FilePath                   -- ^ Cache directory
  -> IORef Int                  -- ^ name store
  -> Block -> IO Block
addRepl cacheDir cp b@(CodeBlock attr@(id', cls, kvpairs) code) = do
  let codeHash = hashCodeBlock attr code

  res <- runMaybeT $ cached cacheDir codeHash $ do
    let langMaps = filterValidAlias cls
    -- no valid language found
    when (null langMaps) $ fail ""

    -- extract highlight lines from attrs
    let hlLines = maybe "" (\c -> ",hl_lines=\"" <> c <> "\"") $ lookup "hl_lines" kvpairs

    -- call pygments to render the code
    html <- io $ readProcess "pygmentize"
      [ "-l", getAlias langMaps
      , "-O", "wrapcode,cssclass=" <> toString hlLines
      , "-f", "html"
      ] $ toString code
    return $! addAttrs id' ("sourceCode":cls) kvpairs $ fromString html

  case res of
    Nothing   -> return b         -- keep block if error
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
