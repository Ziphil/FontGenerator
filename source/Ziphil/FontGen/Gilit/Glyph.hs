{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Glyph
  ( glyphs
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.GlyphUtil
import Ziphil.FontGen.Gilit.Part
import Ziphil.FontGen.Gilit.PartFunc
import Ziphil.FontGen.Gilit.Value


glyphs :: Given Config => Glyphs
glyphs = makeGlyphs list
  where
    list = do
      's' >- glyphUpSal; 'S' >- glyphDownSal
      'z' >- glyphUpZol; 'Z' >- glyphDownZol
      't' >- glyphUpTal; 'T' >- glyphDownTal
      'd' >- glyphUpDol; 'D' >- glyphDownDol
      'k' >- glyphUpKal; 'K' >- glyphDownKal
      'g' >- glyphUpGol; 'G' >- glyphDownGol
      'f' >- glyphUpFal; 'F' >- glyphDownFal
      'v' >- glyphUpVol; 'V' >- glyphDownVol
      'p' >- glyphUpPal; 'P' >- glyphDownPal
      'b' >- glyphUpBol; 'B' >- glyphDownBol
      'c' >- glyphUpCal; 'C' >- glyphDownCal
      'q' >- glyphUpQol; 'Q' >- glyphDownQol
      'x' >- glyphUpXal; 'X' >- glyphDownXal
      'j' >- glyphUpJol; 'J' >- glyphDownJol
      'l' >- glyphUpLes; 'L' >- glyphDownLes
      'r' >- glyphUpRes; 'R' >- glyphDownRes
      'n' >- glyphUpNes; 'N' >- glyphDownNes
      'm' >- glyphUpMes; 'M' >- glyphDownMes
      'y' >- glyphUpYes; 'Y' >- glyphDownYes
      'h' >- glyphUpHes; 'H' >- glyphDownHes
      'w' >- glyphUpTransphone; 'W' >- glyphDownTransphone
      'a' >- glyphUpAt; 'A' >- glyphDownAt
      'á' >- glyphUpAtAcute; 'Á' >- glyphDownAtAcute
      'à' >- glyphUpAtGrave; 'À' >- glyphDownAtGrave
      'â' >- glyphUpAtCircumflex; 'Â' >- glyphDownAtCircumflex
      'e' >- glyphUpEt; 'E' >- glyphDownEtCircumflex
      'é' >- glyphUpEtAcute; 'É' >- glyphDownEtAcute
      'è' >- glyphUpEtGrave; 'È' >- glyphDownEtGrave
      'ê' >- glyphUpEtCircumflex; 'Ê' >- glyphDownEtCircumflex
      'i' >- glyphUpIt; 'I' >- glyphDownIt
      'í' >- glyphUpItAcute; 'Í' >- glyphDownItAcute
      'ì' >- glyphUpItGrave; 'Ì' >- glyphDownItGrave
      'î' >- glyphUpItCircumflex; 'Î' >- glyphDownItCircumflex
      'o' >- glyphUpOt; 'O' >- glyphDownOt
      'ò' >- glyphUpOtGrave; 'Ò' >- glyphDownOtGrave
      'ô' >- glyphUpOtCircumflex; 'Ô' >- glyphDownOtCircumflex
      'u' >- glyphUpUt; 'U' >- glyphDownUt
      'ù' >- glyphUpUtGrave; 'Ù' >- glyphDownUtGrave
      'û' >- glyphUpUtCircumflex; 'Û' >- glyphDownUtCircumflex
      ',' >- glyphTadek
      '.' >- glyphDek
      '!' >- glyphDek
      '?' >- glyphDek
      '\'' >- glyphNok
      'ʻ' >- glyphNok
      ' ' >- glyphSpace

glyphUpSal :: Given Config => Glyph
glyphUpSal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partLeftShortOblique
      partRightShortOblique
      partDiamond

glyphDownSal :: Given Config => Glyph
glyphDownSal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightOblique # transpose

glyphUpZol :: Given Config => Glyph
glyphUpZol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase
      partLeftShortOblique
      partRightShortOblique
      partDiamond
      partTransphone

glyphDownZol :: Given Config => Glyph
glyphDownZol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightOblique # transpose
      partTransphone # transpose

glyphUpTal :: Given Config => Glyph
glyphUpTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique

glyphDownTal :: Given Config => Glyph
glyphDownTal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose

glyphUpDol :: Given Config => Glyph
glyphUpDol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partTransphone

glyphDownDol :: Given Config => Glyph
glyphDownDol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partTransphone # transpose

glyphUpKal :: Given Config => Glyph
glyphUpKal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partLeftAscender

glyphDownKal :: Given Config => Glyph
glyphDownKal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightChippedDescender # transpose

glyphUpGol :: Given Config => Glyph
glyphUpGol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partLeftAscender
      partTransphone

glyphDownGol :: Given Config => Glyph
glyphDownGol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightChippedDescender # transpose
      partTransphone # transpose

glyphUpFal :: Given Config => Glyph
glyphUpFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique

glyphDownFal :: Given Config => Glyph
glyphDownFal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partRightOblique # transpose

glyphUpVol :: Given Config => Glyph
glyphUpVol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partTransphone

glyphDownVol :: Given Config => Glyph
glyphDownVol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase # transpose
      partRightOblique # transpose
      partTransphone # transpose

glyphUpPal :: Given Config => Glyph
glyphUpPal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partRightAscender

glyphDownPal :: Given Config => Glyph
glyphDownPal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftChippedOblique # transpose
      partRightOblique # transpose
      partLeftChippedDescender # transpose

glyphUpBol :: Given Config => Glyph
glyphUpBol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partRightAscender
      partTransphone

glyphDownBol :: Given Config => Glyph
glyphDownBol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftChippedOblique # transpose
      partRightOblique # transpose
      partLeftChippedDescender # transpose
      partTransphone # transpose

glyphUpCal :: Given Config => Glyph
glyphUpCal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftChippedOblique
      partRightOblique
      partLeftChippedDescender

glyphDownCal :: Given Config => Glyph
glyphDownCal = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partRightAscender # transpose

glyphUpQol :: Given Config => Glyph
glyphUpQol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partChippedBase
      partLeftChippedOblique
      partRightOblique
      partLeftChippedDescender
      partTransphone

glyphDownQol :: Given Config => Glyph
glyphDownQol = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partRightAscender # transpose
      partTransphone # transpose

glyphUpXal :: Given Config => Glyph
glyphUpXal = makeGlyphWithSpacing' doubleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftCutOblique
      partRightCenterOblique
      partChippedBase # transpose # translate (triangleWidth / 2 &| 0)
      partRightCutOblique # transpose # translate (triangleWidth / 2 &| 0)

glyphDownXal :: Given Config => Glyph
glyphDownXal = makeGlyphWithSpacing' doubleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftCutOblique # transpose
      partRightCenterOblique # transpose
      partChippedBase # translate (triangleWidth / 2 &| 0)
      partRightCutOblique # translate (triangleWidth / 2 &| 0)

glyphUpJol :: Given Config => Glyph
glyphUpJol = makeGlyphWithSpacing' doubleTransphoneSpacing parts
  where
    parts = do
      partChippedBase
      partLeftCutOblique
      partRightCenterOblique
      partChippedBase # transpose # translate (triangleWidth / 2 &| 0)
      partRightCutOblique # transpose # translate (triangleWidth / 2 &| 0)
      partTransphone # transpose # translate (triangleWidth / 2 &| 0)

glyphDownJol :: Given Config => Glyph
glyphDownJol = makeGlyphWithSpacing' doubleTransphoneSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftCutOblique # transpose
      partRightCenterOblique # transpose
      partChippedBase # translate (triangleWidth / 2 &| 0)
      partRightCutOblique # translate (triangleWidth / 2 &| 0)
      partTransphone # translate (triangleWidth / 2 &| 0)

glyphUpLes :: Given Config => Glyph
glyphUpLes = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftOblique
      partRightChippedOblique
      partRightChippedDescender

glyphDownLes :: Given Config => Glyph
glyphDownLes = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partLeftAscender # transpose

glyphUpRes :: Given Config => Glyph
glyphUpRes = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partChippedBase
      partLeftOblique
      partRightChippedOblique
      partRightChippedDescender
      partTransphone

glyphDownRes :: Given Config => Glyph
glyphDownRes = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partLeftAscender # transpose
      partTransphone # transpose

glyphUpNes :: Given Config => Glyph
glyphUpNes = makeGlyphWithSpacing' doubleSpacing parts
  where
    parts = do
      partBase
      partRightCenterOblique
      partBase # transpose # translate (triangleWidth / 2 &| 0)

glyphDownNes :: Given Config => Glyph
glyphDownNes = makeGlyphWithSpacing' doubleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightOblique # translate (triangleWidth / 2 &| 0)

glyphUpMes :: Given Config => Glyph
glyphUpMes = makeGlyphWithSpacing' doubleTransphoneSpacing parts
  where
    parts = do
      partBase
      partRightCenterOblique
      partBase # transpose # translate (triangleWidth / 2 &| 0)
      partTransphone # transpose # translate (triangleWidth / 2 &| 0)

glyphDownMes :: Given Config => Glyph
glyphDownMes = makeGlyphWithSpacing' doubleTransphoneSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightOblique # translate (triangleWidth / 2 &| 0)
      partTransphone # translate (triangleWidth / 2 &| 0)

glyphUpYes :: Given Config => Glyph
glyphUpYes = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftOblique
      partRightOblique

glyphDownYes :: Given Config => Glyph
glyphDownYes = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftShortOblique # transpose
      partRightShortOblique # transpose
      partDiamond # transpose

glyphUpHes :: Given Config => Glyph
glyphUpHes = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partLeftOblique
      partRightOblique
      partTransphone

glyphDownHes :: Given Config => Glyph
glyphDownHes = makeGlyphWithSpacing' singleTransphoneSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftShortOblique # transpose
      partRightShortOblique # transpose
      partDiamond # transpose
      partTransphone # transpose

glyphUpTransphone :: Given Config => Glyph
glyphUpTransphone = makeGlyphWithSpacing' spacing parts
  where
    parts = do
      partTransphone # transpose # translate (-triangleWidth / 2 - horizontalTransphoneGap - thickness / sinA obliqueAngle &| 0)
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2

glyphDownTransphone :: Given Config => Glyph
glyphDownTransphone = makeGlyphWithSpacing' spacing parts
  where
    parts = do
      partTransphone # translate (-triangleWidth / 2 - horizontalTransphoneGap - thickness / sinA obliqueAngle &| 0)
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2

glyphUpAt :: Given Config => Glyph
glyphUpAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique

glyphDownAt :: Given Config => Glyph
glyphDownAt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose

glyphUpAtAcute :: Given Config => Glyph
glyphUpAtAcute = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partUpperAcute

glyphDownAtAcute :: Given Config => Glyph
glyphDownAtAcute = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partLowerAcute # transpose
      
glyphUpAtGrave :: Given Config => Glyph
glyphUpAtGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partUpperGrave

glyphDownAtGrave :: Given Config => Glyph
glyphDownAtGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partLowerGrave # transpose

glyphUpAtCircumflex :: Given Config => Glyph
glyphUpAtCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partUpperCircumflex

glyphDownAtCircumflex :: Given Config => Glyph
glyphDownAtCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partLowerCircumflex # transpose

glyphUpEt :: Given Config => Glyph
glyphUpEt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partLeftAscender

glyphDownEt :: Given Config => Glyph
glyphDownEt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightChippedDescender # transpose

glyphUpEtAcute :: Given Config => Glyph
glyphUpEtAcute = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partLeftAscender
      partLowerAcute

glyphDownEtAcute :: Given Config => Glyph
glyphDownEtAcute = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightChippedDescender # transpose
      partUpperAcute # transpose

glyphUpEtGrave :: Given Config => Glyph
glyphUpEtGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partLeftAscender
      partLowerGrave

glyphDownEtGrave :: Given Config => Glyph
glyphDownEtGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightChippedDescender # transpose
      partUpperGrave # transpose

glyphUpEtCircumflex :: Given Config => Glyph
glyphUpEtCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partLeftAscender
      partLowerCircumflex

glyphDownEtCircumflex :: Given Config => Glyph
glyphDownEtCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightChippedDescender # transpose
      partUpperCircumflex # transpose

glyphUpIt :: Given Config => Glyph
glyphUpIt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftChippedOblique
      partRightOblique
      partLeftChippedDescender

glyphDownIt :: Given Config => Glyph
glyphDownIt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightAscender # transpose

glyphUpItAcute :: Given Config => Glyph
glyphUpItAcute = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftChippedOblique
      partRightOblique
      partLeftChippedDescender
      partUpperAcute

glyphDownItAcute :: Given Config => Glyph
glyphDownItAcute = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightAscender # transpose
      partLowerAcute # transpose

glyphUpItGrave :: Given Config => Glyph
glyphUpItGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftChippedOblique
      partRightOblique
      partLeftChippedDescender
      partUpperGrave

glyphDownItGrave :: Given Config => Glyph
glyphDownItGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightAscender # transpose
      partLowerGrave # transpose

glyphUpItCircumflex :: Given Config => Glyph
glyphUpItCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partLeftChippedOblique
      partRightOblique
      partLeftChippedDescender
      partUpperCircumflex

glyphDownItCircumflex :: Given Config => Glyph
glyphDownItCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightAscender # transpose
      partLowerCircumflex # transpose

glyphUpOt :: Given Config => Glyph
glyphUpOt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partRightAscender

glyphDownOt :: Given Config => Glyph
glyphDownOt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partRightOblique # transpose
      partLeftDescender # transpose

glyphUpOtGrave :: Given Config => Glyph
glyphUpOtGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partRightAscender
      partLowerGrave

glyphDownOtGrave :: Given Config => Glyph
glyphDownOtGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partRightOblique # transpose
      partLeftDescender # transpose
      partUpperGrave # transpose

glyphUpOtCircumflex :: Given Config => Glyph
glyphUpOtCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partRightAscender
      partLowerCircumflex

glyphDownOtCircumflex :: Given Config => Glyph
glyphDownOtCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partRightOblique # transpose
      partLeftDescender # transpose
      partUpperCircumflex # transpose

glyphUpUt :: Given Config => Glyph
glyphUpUt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftOblique
      partRightDescender

glyphDownUt :: Given Config => Glyph
glyphDownUt = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partLeftAscender # transpose

glyphUpUtGrave :: Given Config => Glyph
glyphUpUtGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftOblique
      partRightDescender
      partUpperGrave

glyphDownUtGrave :: Given Config => Glyph
glyphDownUtGrave = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partLeftAscender # transpose
      partLowerGrave # transpose

glyphUpUtCircumflex :: Given Config => Glyph
glyphUpUtCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftOblique
      partRightDescender
      partUpperCircumflex

glyphDownUtCircumflex :: Given Config => Glyph
glyphDownUtCircumflex = makeGlyphWithSpacing' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partLeftAscender # transpose
      partLowerCircumflex # transpose

glyphTadek :: Given Config => Glyph
glyphTadek = makeGlyphWithSpacing' spacing parts
  where
    parts = do
      partDot
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2

glyphDek :: Given Config => Glyph
glyphDek = makeGlyphWithSpacing' spacing parts
  where
    parts = do
      partDot
      partDot # translate (thickness / sinA obliqueAngle &| 0)
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2 + thickness / sinA obliqueAngle

glyphNok :: Given Config => Glyph
glyphNok = makeGlyphWithSpacing' spacing parts
  where
    parts = do
      partDot
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2

glyphSpace :: Given Config => Glyph
glyphSpace = makeGlyphWithSpacing' spacing skip
  where
    spacing = with &~ do
      fixedWidth .= spaceWidth