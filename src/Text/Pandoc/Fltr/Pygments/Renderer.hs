{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | The renderer of Pygments filter
module Text.Pandoc.Fltr.Pygments.Renderer (
  -- * Helpers
  io,
  cached,
  hashCodeBlock,
  filterValidAlias,
  getAlias,
  -- * Actual renderer
  highlightCodeRaw
) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Control.DeepSeq                          (NFData)
import Control.Monad                            (when)
import Control.Monad.Trans.Maybe
import Data.Map                                 ((!?))
import Data.Maybe                               (mapMaybe)
import Data.Text                                (Text)
import Libkst.Hash
import Libkst.IO
import System.FilePath                          ((<.>), (</>))
import System.Process                           (readProcess)
import Text.Pandoc.Definition
import Text.Pandoc.Fltr.Pygments.Lexers
import Text.Pandoc.Fltr.Pygments.PostProcessors
import Text.Pandoc.Utils

-- | Perform strict IO action then convert to 'MaybeT'
io :: NFData a
   => IO a        -- ^ IO action
   -> MaybeT IO a
io = exceptToMaybeT . tryIODeep
{-# INLINE io #-}

-- A filesystem cache for code snippets
cached
  :: FilePath        -- ^ Cache directory
  -> FilePath        -- ^ File hash
  -> MaybeT IO Text  -- ^ Action
  -> MaybeT IO Text
cached dir hash action = do
  let cachePath = dir </> hash <.> "html"
  -- only compile a new one if no cache exists
  TIO.readFile cachePath `orElseIO'` do
    result <- action
    io $ writeFileHandleMissing' cachePath result
    return result

-- | Hash the code block as a string
hashCodeBlock
  :: Attr     -- ^ attrs
  -> Text     -- ^ code
  -> FilePath -- ^ hash
hashCodeBlock (id', cls, pairs) code = hashText' go
  where
    -- should be unique enough for me
    go = id' <> "◊" <> clsStr <> "◊" <> pairsStr <> "\n" <> code
    clsStr = T.intercalate "¤" cls
    pairsStr = T.intercalate "Δ" $ map (\(k,v) -> "«" <> k <> "↔" <> v <> "»") pairs

-- | Filter all the valid languages from classes and make a list of valid
-- langauge maps.
filterValidAlias
  :: [Text]        -- ^ classes
  -> [(Text,Text)] -- ^ [(alias, lang)]
filterValidAlias = mapMaybe lookupMapping
  where
    lookupMapping :: Text -> Maybe (Text,Text)
    lookupMapping c = (c,) <$> (langMapping !? c)

-- | Get the alias from language map
getAlias :: [(Text,Text)] -> String
getAlias = toString . fst. head
{-# INLINE getAlias #-}

-- | Highlight code snippet using pygments with raw output
highlightCodeRaw
  :: FilePath        -- ^ Cache directory
  -> Attr            -- ^ Attributes
  -> Text            -- ^ Code
  -> IO (Maybe Text) -- ^ Rendered html
highlightCodeRaw cacheDir attr@(id', cls, kvpairs) code = do
  let codeHash = hashCodeBlock attr code

  runMaybeT $ cached cacheDir codeHash $ do
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
