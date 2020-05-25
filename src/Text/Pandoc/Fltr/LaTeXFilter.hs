{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | A filter to transform TeX strings to svg images
module Text.Pandoc.Fltr.LaTeXFilter (
  latexFilterInline,
  latexFilterBlock,
) where

import Text.Pandoc.Fltr.LaTeX.Definitions
import Text.Pandoc.Fltr.LaTeX.EnvOpts
import Text.Pandoc.Fltr.LaTeX.PostProcessors
import Text.Pandoc.Fltr.LaTeX.Renderer

import qualified Data.Text as T

import Data.Text                (Text)
import Libkst.Html
import Numeric                  (showFFloat)
import System.FilePath          ((</>))
import Text.Pandoc.Definition
import Text.Pandoc.Filter.Utils
import Text.Pandoc.Utils

-- * Utils

-- | Render errors nicely, in order to show any problems clearly, with all
-- information intact.
displayError :: RenderError -> Inline
displayError (LaTeXFailure str doc) =
  RawInline (Format "html") e
    where
      e = "<pre class=\"err\">LaTeX failed.\n" <> toText str <> "\nGenerated document:\n" <> doc <> "</pre>"
displayError (DVISVGMFailure str) =
  RawInline (Format "html") e
    where
      e = "<pre class=\"err\">dvisvgm failed.\n" <> toText str <> "</pre>"
displayError (IOException ex) =
  RawInline (Format "html") e
    where
      e = "<pre class=\"err\">IO exception.\n" <> toText (show ex) <> "</pre>"

-- | Render a TeXStr to SVG
renderTeXStr
  :: String         -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> TeXString      -- ^ the tex string to be rendered
  -> IO (Maybe LaTeXEnv, Either RenderError SVG)
renderTeXStr docId math texStr = do
  let cacheDir = "cache" </> docId
  let (env, texDoc) = case math of
       Just InlineMath -> (Just "math", mkMathTeXDoc InlineMath texStr)
       Just DisplayMath -> (Just "displaymath", mkMathTeXDoc DisplayMath texStr)
       Nothing -> (e, mkTeXDoc (lookupPreamble e) texStr)
         where
           e = findEnv texStr
  (env,) <$> compileSVG cacheDir texDoc

-- | Render the SVG as an actual inline image element with error handling. Note
-- that we are not displaying the plain SVG inline because there are some
-- serious rendering issues in many browsers
renderHandleError :: Maybe LaTeXEnv -> Either RenderError SVG -> Inline
renderHandleError _ (Left err) = displayError err
renderHandleError env (Right svg) =
  case env of
    Nothing -> Image ("", ["tex", "noenv"], baseAdjust) [] (svgE, "")
    --                    ^ class
    Just e  -> Image ("", ["tex", T.map escapeStar e], baseAdjust) [] (svgE, "")
    where
      -- | escape starred environments, align* -> align_
      escapeStar :: Char -> Char
      escapeStar '*' = '_'
      escapeStar  x  =  x

      -- | post processed svg
      psvg :: SVG
      psvg = postProcessSVG svg

      -- | baseline correction of the image
      baseline :: Double
      baseline = getBaseline psvg

      -- | encoded svg image
      svgE :: Text
      svgE = encodeSVG psvg

      -- | attributes
      baseAdjust :: [(Text, Text)]
      baseAdjust = [("style", "vertical-align:" <> toText (showFFloat (Just 6) baseline "") <> "pt")]

-- * Inline filter

-- | Render a TeXStr to inline SVG
renderInlineSVG
  :: String          -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> T.Text           -- ^ the tex string to be rendered
  -> IO Inline
renderInlineSVG docId mt texStr =
  uncurry renderHandleError <$> renderTeXStr docId mt (T.strip texStr) -- TODO switch to Text instead

-- | Convert inline TeX strings to SVG images
latexFilterInline'
  :: String
  -> Inline
  -> IO Inline
-- math environment
latexFilterInline' docId (Math mathType texStr) =
  renderInlineSVG docId (Just mathType) texStr
-- tex environment
latexFilterInline' docId (RawInline (Format "tex") texStr) =
  renderInlineSVG docId Nothing texStr
latexFilterInline' _ x = return x

-- * Block filter

-- | Render a TeXStr to block SVG
renderBlockSVG
  :: String          -- ^ document id for cache dir
  -> Maybe MathType -- ^ is math environment
  -> T.Text         -- ^ the tex string to be rendered
  -> IO Block
renderBlockSVG docId mt texStr = do
  i <- renderInlineSVG docId mt texStr
  return $ Plain [i]

-- | Convert block TeX strings to SVG images
latexFilterBlock'
  :: String
  -> Block
  -> IO Block
latexFilterBlock' docId (RawBlock (Format "tex") texStr) =
  renderBlockSVG docId Nothing texStr
latexFilterBlock' _ x = return x

-- * partial -> complete filter

latexFilterInline :: String -> PandocFilterM IO
latexFilterInline = mkFilter . latexFilterInline'

latexFilterBlock :: String -> PandocFilterM IO
latexFilterBlock = mkFilter . latexFilterBlock'
