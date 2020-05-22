-- | A QuasiQuoter For cleaner formatting
module Text.Pandoc.Fltr.LaTeX.Quote (kfmt) where

import Language.Haskell.TH.Quote

import PyF

kfmt :: QuasiQuoter
kfmt = fmtWithDelimiters ('<','>')
