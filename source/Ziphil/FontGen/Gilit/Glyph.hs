{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Glyph
  ( glyphs
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.Part
import Ziphil.FontGen.Gilit.Util
import Ziphil.FontGen.Gilit.Value


glyphs :: Given Config => Glyphs
glyphs = makeGlyphs' list
  where
    list =
      [ 'a' >- glyphAt
      , 't' >- glyphTal
      , 'f' >- glyphFal
      ]

glyphAt :: Given Config => Bool -> Glyph
glyphAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique
      , partRightOblique
      , partBase
      ]

glyphTal :: Given Config => Bool -> Glyph
glyphTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique
      , partBase
      ]

glyphFal :: Given Config => Bool -> Glyph
glyphFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partRightOblique
      , partBase
      ]