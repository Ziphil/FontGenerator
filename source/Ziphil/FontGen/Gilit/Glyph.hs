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
      , 'z' >- glyphUpZol, 'Z' >- glyphDownZol
      , 't' >- glyphUpTal, 'T' >- glyphDownTal
      , 'd' >- glyphUpDol, 'D' >- glyphDownDol
      , 'k' >- glyphUpKal, 'K' >- glyphDownKal
      , 'g' >- glyphUpGol, 'G' >- glyphDownGol
      , 'f' >- glyphUpFal, 'F' >- glyphDownFal
      , 'v' >- glyphUpVol, 'V' >- glyphDownVol
      , 'p' >- glyphUpPal, 'P' >- glyphDownPal
      , 'b' >- glyphUpBol, 'B' >- glyphDownBol
      , 'c' >- glyphUpCal, 'C' >- glyphDownCal
      , 'q' >- glyphUpQol, 'Q' >- glyphDownQol
      , 'l' >- glyphUpLes, 'L' >- glyphDownLes
      , 'r' >- glyphUpRes, 'R' >- glyphDownRes
      , 'y' >- glyphUpYes, 'Y' >- glyphDownYes
      , 'h' >- glyphUpHes, 'H' >- glyphDownHes
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

glyphUpZol :: Given Config => Glyph
glyphUpZol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase
      , partLeftShortOblique
      , partRightShortOblique
      , partDiamond
      , partTransphone
      ]

glyphDownZol :: Given Config => Glyph
glyphDownZol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partLeftOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      , partTransphone # reflectTriangle
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

glyphUpDol :: Given Config => Glyph
glyphUpDol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      , partTransphone
      ]

glyphDownDol :: Given Config => Glyph
glyphDownDol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partTransphone # reflectTriangle
      ]

glyphUpKal :: Given Config => Glyph
glyphUpKal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      , partRightOblique
      , partLeftAscender
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

glyphUpGol :: Given Config => Glyph
glyphUpGol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      , partRightOblique
      , partLeftAscender
      , partTransphone
      ]

glyphDownGol :: Given Config => Glyph
glyphDownGol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partChippedBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partRightChippedOblique # reflectTriangle
      , partRightChippedDescender # reflectTriangle
      , partTransphone # reflectTriangle
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

glyphUpVol :: Given Config => Glyph
glyphUpVol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase
      , partRightOblique
      , partTransphone
      ]

glyphDownVol :: Given Config => Glyph
glyphDownVol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partRightOblique # reflectTriangle
      , partTransphone # reflectTriangle
      ]

glyphUpPal :: Given Config => Glyph
glyphUpPal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      , partRightOblique
      , partRightAscender
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

glyphUpBol :: Given Config => Glyph
glyphUpBol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase
      , partLeftOblique
      , partRightOblique
      , partRightAscender
      , partTransphone
      ]

glyphDownBol :: Given Config => Glyph
glyphDownBol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partChippedBase # reflectTriangle
      , partLeftChippedOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      , partLeftChippedDescender # reflectTriangle
      , partTransphone # reflectTriangle
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
      , partRightAscender # reflectTriangle
      ]

glyphUpQol :: Given Config => Glyph
glyphUpQol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partChippedBase
      , partLeftChippedOblique
      , partRightOblique
      , partLeftChippedDescender
      , partTransphone
      ]

glyphDownQol :: Given Config => Glyph
glyphDownQol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      , partRightAscender # reflectTriangle
      , partTransphone # reflectTriangle
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
      , partLeftAscender # reflectTriangle
      ]

glyphUpRes :: Given Config => Glyph
glyphUpRes = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partChippedBase
      , partLeftOblique
      , partRightChippedOblique
      , partRightChippedDescender
      , partTransphone
      ]

glyphDownRes :: Given Config => Glyph
glyphDownRes = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partRightOblique # reflectTriangle
      , partLeftAscender # reflectTriangle
      , partTransphone # reflectTriangle
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

glyphUpHes :: Given Config => Glyph
glyphUpHes = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partLeftOblique
      , partRightOblique
      , partTransphone
      ]

glyphDownHes :: Given Config => Glyph
glyphDownHes = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftShortOblique # reflectTriangle
      , partRightShortOblique # reflectTriangle
      , partDiamond # reflectTriangle
      , partTransphone # reflectTriangle
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
      [ partRightChippedBase # reflectTriangle
      , partRightChippedOblique # reflectTriangle
      , partRightChippedDescender # reflectTriangle
      ]

glyphUpUt :: Given Config => Glyph
glyphUpUt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftChippedBase
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