{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-
 - Adapted from an early version of [1] with support for responsive SVG,
 - custom preambles, environment parsing, and environment-specific preambles.
 - [1]: https://github.com/phadej/latex-svg
 - 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor
 -}

-- | A filter to transform TeX strings to svg images
module Text.Pandoc.Fltr.LaTeXFilter (
  preambleFilter,
  latexFilter,
  LaTeXFilterOptions(..),
  def,
) where

import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Fltr.LaTeX.DocumentBuilder
import Text.Pandoc.Fltr.LaTeX.PostProcessors
import Text.Pandoc.Fltr.LaTeX.Renderer

import qualified Data.Text as T

import Control.Monad              (when)
import Control.Monad.Trans.Writer
import Data.Default               (def)
import Data.Text                  (Text)
import Libkst.Html
import Text.Pandoc.Definition
import Text.Pandoc.Utils

-- * Utils

-- ** Error display

-- | Render errors nicely, in order to show any problems clearly, with all
-- information intact.
displayError :: RenderError -> Inline
displayError (LaTeXFailure err doc) = mkError $
  "LaTeX failed.\n" <> toText err <> "\nGenerated document:\n" <> doc
displayError (DVISVGMFailure err) = mkError $
  "dvisvgm failed.\n" <> toText err
displayError (IOException ex) = mkError $
  "IO exception.\n" <> toText (show ex)

-- | Make inline element from error message
mkError
  :: Text   -- ^ Error message
  -> Inline -- ^ Inline error display
mkError err = RawInline (Format "html") $
  "<pre class=\"err\">" <> err <> "</pre>"

-- ** Render TeX

-- | Render a TeXStr to SVG
renderTeXStr
  :: LaTeXFilterOptions -- ^ document id for cache dir
  -> Maybe MathType     -- ^ is math environment
  -> TeXString          -- ^ the tex string to be rendered
  -> IO (Maybe LaTeXEnv, Either RenderError SVG)
renderTeXStr opts math texStr = do
  let preamb = extraPreamble opts
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
  -> Maybe MathType     -- ^ is math environment
  -> TeXString          -- ^ the tex string to be rendered
  -> IO Inline
renderInlineSVG opts mt texStr =
  uncurry (renderHandleError opts) <$> renderTeXStr opts mt (T.strip texStr)

-- | Convert inline TeX strings to SVG images
latexFilterInline'
  :: LaTeXFilterOptions -- ^ Render options
  -> Inline -> IO Inline
-- math environment
latexFilterInline' opts (Math mathType texStr) =
  renderInlineSVG opts (Just mathType) texStr
-- tex environment
latexFilterInline' opts (RawInline (Format "tex") texStr) =
  renderInlineSVG opts Nothing texStr
latexFilterInline' _ x = return x

-- * Block filter

-- | Render a TeXStr to block SVG
renderBlockSVG
  :: LaTeXFilterOptions -- ^ Render options
  -> TeXString          -- ^ The tex string to be rendered
  -> IO Block
renderBlockSVG opts texStr = do
  i <- renderInlineSVG opts Nothing texStr
  return $ Plain [i]

-- | Convert block TeX strings to SVG images
latexFilterBlock'
  :: LaTeXFilterOptions -- ^ Render options
  -> Block -> IO Block
latexFilterBlock' opts (RawBlock (Format "tex") texStr) =
  renderBlockSVG opts texStr
latexFilterBlock' _ x = return x

-- * Preamble filter

extractPreamble :: Block -> Writer Text Block
extractPreamble blk@(CodeBlock (_,cls,_) preamb)
  | "preamble" `elem` cls = do
       when ("preamble" `elem` cls) $ tell $ preamb <> "\n"
       return Null
  | otherwise = return blk
extractPreamble x = return x

preambleFilter :: PandocFilterM (Writer Text)
preambleFilter = mkFilter extractPreamble

-- * LaTeX filter

latexFilter'
  :: LaTeXFilterOptions -- ^ Filter options
  -> Pandoc             -- ^ Pandoc document
  -> IO Pandoc          -- ^ result
latexFilter' opts doc = do
  -- extract preamble from document
  let (doc', !preamb) = runWriter $ applyFilterM preambleFilter doc
      newOpts = opts { extraPreamble = extraPreamble opts <> preamb }

  -- make filter
  let ilFilter = mkFilter $ latexFilterInline' newOpts
      blkFilter = mkFilter $ latexFilterBlock' newOpts

  applyFilterM (ilFilter <> blkFilter) doc'

-- | Compile any TeX commands in a document to embedded SVG images
latexFilter
  :: LaTeXFilterOptions -- ^ Filter options
  -> PandocFilterM IO   -- ^ Final filter
latexFilter = mkFilter . latexFilter'
