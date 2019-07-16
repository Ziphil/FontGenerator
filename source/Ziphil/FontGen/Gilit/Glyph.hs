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
glyphs = makeGlyphs list
  where
    list =
      [ 'a' >- glyphUpAt, 'A' >- glyphDownAt
      , 't' >- glyphUpTal, 'T' >- glyphDownTal
      , 'f' >- glyphUpFal, 'F' >- glyphDownFal
      ]

glyphUpAt :: Given Config => Glyph
glyphUpAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftUpOblique
      , partRightUpOblique
      , partUpBase
      ]

glyphDownAt :: Given Config => Glyph
glyphDownAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftDownOblique
      , partRightDownOblique
      , partDownBase
      ]

glyphUpTal :: Given Config => Glyph
glyphUpTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftUpOblique
      , partUpBase
      ]

glyphDownTal :: Given Config => Glyph
glyphDownTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftDownOblique
      , partDownBase
      ]

glyphUpFal :: Given Config => Glyph
glyphUpFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partRightUpOblique
      , partUpBase
      ]

glyphDownFal :: Given Config => Glyph
glyphDownFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partRightDownOblique
      , partDownBase
      ]