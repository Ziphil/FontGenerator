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
      , 'x' >- glyphUpXal, 'X' >- glyphDownXal
      , 'j' >- glyphUpJol, 'J' >- glyphDownJol
      , 'l' >- glyphUpLes, 'L' >- glyphDownLes
      , 'r' >- glyphUpRes, 'R' >- glyphDownRes
      , 'n' >- glyphUpNes, 'N' >- glyphDownNes
      , 'm' >- glyphUpMes, 'M' >- glyphDownMes
      , 'y' >- glyphUpYes, 'Y' >- glyphDownYes
      , 'h' >- glyphUpHes, 'H' >- glyphDownHes
      , 'a' >- glyphUpAt, 'A' >- glyphDownAt
      , 'á' >- glyphUpAt, 'Á' >- glyphDownAt
      , 'à' >- glyphUpAt, 'À' >- glyphDownAt
      , 'â' >- glyphUpAt, 'Â' >- glyphDownAt
      , 'e' >- glyphUpEt, 'E' >- glyphDownEt
      , 'é' >- glyphUpEt, 'É' >- glyphDownEt
      , 'è' >- glyphUpEt, 'È' >- glyphDownEt
      , 'ê' >- glyphUpEt, 'Ê' >- glyphDownEt
      , 'i' >- glyphUpIt, 'I' >- glyphDownIt
      , 'í' >- glyphUpIt, 'Í' >- glyphDownIt
      , 'ì' >- glyphUpIt, 'Ì' >- glyphDownIt
      , 'î' >- glyphUpIt, 'Î' >- glyphDownIt
      , 'o' >- glyphUpOt, 'O' >- glyphDownOt
      , 'ò' >- glyphUpOt, 'Ò' >- glyphDownOt
      , 'ô' >- glyphUpOt, 'Ô' >- glyphDownOt
      , 'u' >- glyphUpUt, 'U' >- glyphDownUt
      , 'ù' >- glyphUpUt, 'Ù' >- glyphDownUt
      , 'û' >- glyphUpUt, 'Û' >- glyphDownUt
      , ',' >- glyphTadek
      , '.' >- glyphDek
      , '!' >- glyphDek
      , '?' >- glyphDek
      , '\'' >- glyphNok
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

glyphUpXal :: Given Config => Glyph
glyphUpXal = makeGlyphWithSpacing' doubleSpacing parts
  where
    parts = 
      [ partChippedBase
      , partLeftCutOblique
      , partRightCenterOblique
      , partChippedBase # reflectTriangle # translate (triangleWidth / 2 &| 0)
      , partRightCutOblique # reflectTriangle # translate (triangleWidth / 2 &| 0)
      ]

glyphDownXal :: Given Config => Glyph
glyphDownXal = makeGlyphWithSpacing' doubleSpacing parts
  where
    parts = 
      [ partChippedBase # reflectTriangle
      , partLeftCutOblique # reflectTriangle
      , partRightCenterOblique # reflectTriangle
      , partChippedBase # translate (triangleWidth / 2 &| 0)
      , partRightCutOblique # translate (triangleWidth / 2 &| 0)
      ]

glyphUpJol :: Given Config => Glyph
glyphUpJol = makeGlyphWithSpacing' doubleTransphoneSpacing parts
  where
    parts = 
      [ partChippedBase
      , partLeftCutOblique
      , partRightCenterOblique
      , partChippedBase # reflectTriangle # translate (triangleWidth / 2 &| 0)
      , partRightCutOblique # reflectTriangle # translate (triangleWidth / 2 &| 0)
      , partTransphone # reflectTriangle # translate (triangleWidth / 2 &| 0)
      ]

glyphDownJol :: Given Config => Glyph
glyphDownJol = makeGlyphWithSpacing' doubleTransphoneSpacing parts
  where
    parts = 
      [ partChippedBase # reflectTriangle
      , partLeftCutOblique # reflectTriangle
      , partRightCenterOblique # reflectTriangle
      , partChippedBase # translate (triangleWidth / 2 &| 0)
      , partRightCutOblique # translate (triangleWidth / 2 &| 0)
      , partTransphone # translate (triangleWidth / 2 &| 0)
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

glyphUpNes :: Given Config => Glyph
glyphUpNes = makeGlyphWithSpacing' doubleSpacing parts
  where
    parts = 
      [ partBase
      , partRightCenterOblique
      , partBase # reflectTriangle # translate (triangleWidth / 2 &| 0)
      ]

glyphDownNes :: Given Config => Glyph
glyphDownNes = makeGlyphWithSpacing' doubleSpacing parts
  where
    parts = 
      [ partLeftOblique # reflectTriangle
      , partRightChippedOblique # reflectTriangle
      , partRightOblique # translate (triangleWidth / 2 &| 0)
      ]

glyphUpMes :: Given Config => Glyph
glyphUpMes = makeGlyphWithSpacing' doubleTransphoneSpacing parts
  where
    parts = 
      [ partBase
      , partRightCenterOblique
      , partBase # reflectTriangle # translate (triangleWidth / 2 &| 0)
      , partTransphone # reflectTriangle # translate (triangleWidth / 2 &| 0)
      ]

glyphDownMes :: Given Config => Glyph
glyphDownMes = makeGlyphWithSpacing' doubleTransphoneSpacing parts
  where
    parts = 
      [ partLeftOblique # reflectTriangle
      , partRightChippedOblique # reflectTriangle
      , partRightOblique # translate (triangleWidth / 2 &| 0)
      , partTransphone # translate (triangleWidth / 2 &| 0)
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
      , partLeftAscender
      ]

glyphDownEt :: Given Config => Glyph
glyphDownEt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftOblique # reflectTriangle
      , partRightChippedOblique # reflectTriangle
      , partRightChippedDescender # reflectTriangle
      ]

glyphUpIt :: Given Config => Glyph
glyphUpIt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partLeftChippedOblique
      , partRightOblique
      , partLeftChippedDescender
      ]

glyphDownIt :: Given Config => Glyph
glyphDownIt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partRightAscender # reflectTriangle
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
      , partRightOblique # reflectTriangle
      , partLeftDescender # reflectTriangle
      ]

glyphUpUt :: Given Config => Glyph
glyphUpUt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partChippedBase
      , partLeftOblique
      , partRightDescender
      ]

glyphDownUt :: Given Config => Glyph
glyphDownUt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = 
      [ partBase # reflectTriangle
      , partLeftOblique # reflectTriangle
      , partLeftAscender # reflectTriangle
      ]

glyphTadek :: Given Config => Glyph
glyphTadek = makeGlyphWithSpacing' spacing parts
  where
    parts =
      [ partDot
      ]
    spacing = spacingBy $ triangleWidth / 2

glyphDek :: Given Config => Glyph
glyphDek = makeGlyphWithSpacing' spacing parts
  where
    parts =
      [ partDot
      , partDot # translate (thickness / sinA obliqueAngle &| 0)
      ]
    spacing = spacingBy $ triangleWidth / 2 + thickness / sinA obliqueAngle

glyphNok :: Given Config => Glyph
glyphNok = makeGlyphWithSpacing' spacing parts
  where
    parts =
      [ partDot
      ]
    spacing = spacingBy $ triangleWidth / 2

glyphSpace :: Given Config => Glyph
glyphSpace = makeGlyphWithSpacing' spacing []
  where
    spacing = with &~ do
      leftX .= 0
      fixedWidth .= spaceWidth