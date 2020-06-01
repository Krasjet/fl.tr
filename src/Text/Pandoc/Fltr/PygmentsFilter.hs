{-# LANGUAGE ScopedTypeVariables #-}

-- | Use Pygments to highlight code instead
module Text.Pandoc.Fltr.PygmentsFilter (
  pygmentsFilter
) where

import Text.Pandoc.Fltr.Pygments.Renderer
import Text.Pandoc.Utils

pygmentsFilter
  :: FilePath         -- ^ Cache directory
  -> PandocFilterM IO
pygmentsFilter cache = mkFilter $ highlightCodeWith cache $ const id
