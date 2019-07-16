{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Glyph
  ( glyphs
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.GlyphUtil
import Ziphil.FontGen.Gilit.Part
import Ziphil.FontGen.Gilit.Value


glyphs :: Given Config => Glyphs
glyphs = makeGlyphs list
  where
    list =
      [ 's' >- glyphUpSal, 'S' >- glyphDownSal
      , 't' >- glyphUpTal, 'T' >- glyphDownTal
      , 'k' >- glyphUpKal, 'K' >- glyphDownKal
      , 'f' >- glyphUpFal, 'F' >- glyphDownFal
      , 'p' >- glyphUpPal, 'P' >- glyphDownPal
      , 'c' >- glyphUpCal, 'C' >- glyphDownCal
      , 'l' >- glyphUpLes, 'L' >- glyphDownLes
      , 'y' >- glyphUpYes, 'Y' >- glyphDownYes
      , 'a' >- glyphUpAt, 'A' >- glyphDownAt
      , 'e' >- glyphUpEt, 'E' >- glyphDownEt
      , 'i' >- glyphUpIt, 'I' >- glyphDownIt
      , 'o' >- glyphUpOt, 'O' >- glyphDownOt
      , 'u' >- glyphUpUt, 'U' >- glyphDownUt
      , ' ' >- glyphSpace
      ]

glyphUpSal :: Given Config => Glyph
glyphUpSal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partLeftShortOblique
      , partRightShortOblique
      , partDiamond
      ]

glyphDownSal :: Given Config => Glyph
glyphDownSal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      ]

glyphUpTal :: Given Config => Glyph
glyphUpTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      ]

glyphDownTal :: Given Config => Glyph
glyphDownTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      ]

glyphUpKal :: Given Config => Glyph
glyphUpKal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      , partRightOblique
      , partRightAscender
      ]

glyphDownKal :: Given Config => Glyph
glyphDownKal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partRightChippedOblique # reflectTriangle
      , partRightChippedDescender # reflectTriangle
      ]

glyphUpFal :: Given Config => Glyph
glyphUpFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partRightOblique
      ]

glyphDownFal :: Given Config => Glyph
glyphDownFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partRightOblique # reflectTriangle
      ]

glyphUpPal :: Given Config => Glyph
glyphUpPal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      , partRightOblique
      , partLeftAscender
      ]

glyphDownPal :: Given Config => Glyph
glyphDownPal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase # reflectTriangle
      , partLeftChippedOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      , partLeftChippedDescender # reflectTriangle
      ]

glyphUpCal :: Given Config => Glyph
glyphUpCal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase
      , partLeftChippedOblique
      , partRightOblique
      , partLeftChippedDescender
      ]

glyphDownCal :: Given Config => Glyph
glyphDownCal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      , partLeftAscender # reflectTriangle
      ]

glyphUpLes :: Given Config => Glyph
glyphUpLes = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase
      , partLeftOblique
      , partRightChippedOblique
      , partRightChippedDescender
      ]

glyphDownLes :: Given Config => Glyph
glyphDownLes = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      , partRightAscender # reflectTriangle
      ]

glyphUpYes :: Given Config => Glyph
glyphUpYes = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique
      , partRightOblique
      ]

glyphDownYes :: Given Config => Glyph
glyphDownYes = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftShortOblique # reflectTriangle
      , partRightShortOblique # reflectTriangle
      , partDiamond # reflectTriangle
      ]

glyphUpAt :: Given Config => Glyph
glyphUpAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      , partRightOblique
      ]

glyphDownAt :: Given Config => Glyph
glyphDownAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      ]

glyphUpEt :: Given Config => Glyph
glyphUpEt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partRightOblique
      , partRightAscender
      ]

glyphDownEt :: Given Config => Glyph
glyphDownEt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase # reflectTriangle
      , partRightOblique # reflectTriangle
      , partLeftDescender # reflectTriangle
      ]

glyphUpIt :: Given Config => Glyph
glyphUpIt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase
      , partRightOblique
      , partLeftDescender
      ]

glyphDownIt :: Given Config => Glyph
glyphDownIt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partLeftAscender # reflectTriangle
      ]

glyphUpOt :: Given Config => Glyph
glyphUpOt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partRightOblique
      , partRightAscender
      ]

glyphDownOt :: Given Config => Glyph
glyphDownOt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase # reflectTriangle
      , partRightChippedOblique # reflectTriangle
      , partRightChippedDescender # reflectTriangle
      ]

glyphUpUt :: Given Config => Glyph
glyphUpUt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase
      , partLeftChippedOblique
      , partLeftChippedDescender
      ]

glyphDownUt :: Given Config => Glyph
glyphDownUt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partLeftAscender # reflectTriangle
      ]

glyphSpace :: Given Config => Glyph
glyphSpace = makeGlyphWithSpacing' spacing []
  where
    spacing = with &~ do
      leftX .= 0
      fixedWidth .= spaceWidth