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
      , 'e' >- glyphUpEt
      , 'k' >- glyphUpKal
      ]

glyphUpAt :: Given Config => Glyph
glyphUpAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique
      , partRightOblique
      , partBase
      ]

glyphDownAt :: Given Config => Glyph
glyphDownAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      , partBase # reflectTriangle
      ]

glyphUpTal :: Given Config => Glyph
glyphUpTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique
      , partBase
      ]

glyphDownTal :: Given Config => Glyph
glyphDownTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique # reflectTriangle
      , partBase # reflectTriangle
      ]

glyphUpFal :: Given Config => Glyph
glyphUpFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partRightOblique
      , partBase
      ]

glyphDownFal :: Given Config => Glyph
glyphDownFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partRightOblique # reflectTriangle
      , partBase # reflectTriangle
      ]

glyphUpEt :: Given Config => Glyph
glyphUpEt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partRightAscender
      , partRightOblique
      , partBase
      ]

glyphUpKal :: Given Config => Glyph
glyphUpKal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftAscender
      , partLeftOblique
      , partRightOblique
      , partBase
      ]