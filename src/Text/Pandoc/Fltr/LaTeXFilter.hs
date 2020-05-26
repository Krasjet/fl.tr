{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | A filter to transform TeX strings to svg images
module Text.Pandoc.Fltr.LaTeXFilter (
  preambleFilter,
  latexFilter
) where

import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Fltr.LaTeX.DocumentBuilder
import Text.Pandoc.Fltr.LaTeX.PostProcessors
import Text.Pandoc.Fltr.LaTeX.Renderer

import qualified Data.Text as T

import Control.Monad              (when)
import Control.Monad.Trans.Writer
import Data.Text                  (Text)
import Libkst.Html
import Text.Pandoc.Definition
import Text.Pandoc.Filter.Utils
import Text.Pandoc.Utils

-- * Utils

-- | Render errors nicely, in order to show any problems clearly, with all
-- information intact.
displayError :: RenderError -> Inline
displayError (LaTeXFailure str doc) = RawInline (Format "html") e
    where
      e = "<pre class=\"err\">LaTeX failed.\n" <> toText str <> "\nGenerated document:\n" <> doc <> "</pre>"
displayError (DVISVGMFailure str) = RawInline (Format "html") e
    where
      e = "<pre class=\"err\">dvisvgm failed.\n" <> toText str <> "</pre>"
displayError (IOException ex) = RawInline (Format "html") e
    where
      e = "<pre class=\"err\">IO exception.\n" <> toText (show ex) <> "</pre>"

-- | Render a TeXStr to SVG
renderTeXStr
  :: LaTeXFilterOptions -- ^ document id for cache dir
  -> Preamble           -- ^ extra preambles
  -> Maybe MathType     -- ^ is math environment
  -> TeXString          -- ^ the tex string to be rendered
  -> IO (Maybe LaTeXEnv, Either RenderError SVG)
renderTeXStr opts preamb math texStr = do
  let (env, texDoc) = case math of
       Just InlineMath -> (Just "math", mkMathDocument InlineMath preamb texStr)
       Just DisplayMath -> (Just "displaymath", mkMathDocument DisplayMath preamb texStr)
       Nothing -> (e, mkTeXDocument (lookupPreamble e <> preamb) texStr)
         where
           e = extractEnv texStr
  (env,) <$> compileSVG opts texDoc

-- | Render the SVG as an actual inline image element with error handling. Note
-- that we are not displaying the plain SVG inline because there are some
-- serious rendering issues in many browsers
renderHandleError
  :: LaTeXFilterOptions -- ^ Render options
  -> Maybe LaTeXEnv
  -> Either RenderError SVG
  -> Inline
renderHandleError _ _ (Left err) = displayError err
renderHandleError opts env (Right svg) =
  case env of
    Nothing ->  Image (imgAttrs `addClasses` ["tex", "noenv"]) [] (svgEncoded, "")
    Just e  -> Image (imgAttrs `addClasses` ["tex", T.map escapeStar e]) [] (svgEncoded, "")
    where
      -- | escape starred environments, align* -> align_
      escapeStar :: Char -> Char
      escapeStar '*' = '_'
      escapeStar  x  =  x

      -- | post processed svg
      psvg :: SVG
      psvg = postProcessSVG svg

      -- | baseline correction of the image
      imgAttrs :: Attr
      imgAttrs = getImgAttr (baseFontSize opts) psvg

      -- | encoded svg image
      svgEncoded :: Text
      svgEncoded = encodeSVG psvg

-- * Inline filter

-- | Render a TeXStr to inline SVG
renderInlineSVG
  :: LaTeXFilterOptions -- ^ Render options
  -> Preamble           -- ^ Extra preambles
  -> Maybe MathType     -- ^ is math environment
  -> TeXString          -- ^ the tex string to be rendered
  -> IO Inline
renderInlineSVG opts preamb mt texStr =
  uncurry (renderHandleError opts) <$> renderTeXStr opts preamb mt (T.strip texStr)

-- | Convert inline TeX strings to SVG images
latexFilterInline'
  :: LaTeXFilterOptions -- ^ Render options
  -> Preamble           -- ^ Extra preambles
  -> Inline -> IO Inline
-- math environment
latexFilterInline' opts preamb (Math mathType texStr) =
  renderInlineSVG opts preamb (Just mathType) texStr
-- tex environment
latexFilterInline' opts preamb (RawInline (Format "tex") texStr) =
  renderInlineSVG opts preamb Nothing texStr
latexFilterInline' _ _ x = return x

-- * Block filter

-- | Render a TeXStr to block SVG
renderBlockSVG
  :: LaTeXFilterOptions -- ^ Render options
  -> Preamble           -- ^ Extra preambles
  -> TeXString          -- ^ The tex string to be rendered
  -> IO Block
renderBlockSVG opts preamb texStr = do
  i <- renderInlineSVG opts preamb Nothing texStr
  return $ Plain [i]

-- | Convert block TeX strings to SVG images
latexFilterBlock'
  :: LaTeXFilterOptions -- ^ Render options
  -> Preamble           -- ^ Extra preambles
  -> Block -> IO Block
latexFilterBlock' opts preamb (RawBlock (Format "tex") texStr) =
  renderBlockSVG opts preamb texStr
latexFilterBlock' _ _ x = return x

-- * Preamble filter

extractPreamble :: Block -> Writer Text Block
extractPreamble (CodeBlock (_,cls,_) preamb) = do
  when ("preamble" `elem` cls) $ tell $ preamb <> "\n"
  return Null
extractPreamble x = return x

preambleFilter :: PandocFilterM (Writer Text)
preambleFilter = mkFilter extractPreamble

-- * The actual filter

latexFilter'
  :: LaTeXFilterOptions -- ^ Filter options
  -> Pandoc             -- ^ Pandoc document
  -> IO Pandoc          -- ^ result
latexFilter' opts doc = do
  -- extract preamble from document
  let (!doc', !preamb) = runWriter $ applyFilterM preambleFilter doc
  applyFiltersM
    [mkFilter $ latexFilterInline' opts preamb, mkFilter $ latexFilterBlock' opts preamb] doc'

-- | Compile any TeX commands in a document to embedded SVG images
latexFilter
  :: LaTeXFilterOptions -- ^ Filter options
  -> PandocFilterM IO   -- ^ Final filter
latexFilter = mkFilter . latexFilter'
