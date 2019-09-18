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
glyphs = glyphsBy list
  where
    list = do
      's' @= glyphUpSal; 'S' @= glyphDownSal
      'z' @= glyphUpZol; 'Z' @= glyphDownZol
      't' @= glyphUpTal; 'T' @= glyphDownTal
      'd' @= glyphUpDol; 'D' @= glyphDownDol
      'k' @= glyphUpKal; 'K' @= glyphDownKal
      'g' @= glyphUpGol; 'G' @= glyphDownGol
      'f' @= glyphUpFal; 'F' @= glyphDownFal
      'v' @= glyphUpVol; 'V' @= glyphDownVol
      'p' @= glyphUpPal; 'P' @= glyphDownPal
      'b' @= glyphUpBol; 'B' @= glyphDownBol
      'c' @= glyphUpCal; 'C' @= glyphDownCal
      'q' @= glyphUpQol; 'Q' @= glyphDownQol
      'x' @= glyphUpXal; 'X' @= glyphDownXal
      'j' @= glyphUpJol; 'J' @= glyphDownJol
      'l' @= glyphUpLes; 'L' @= glyphDownLes
      'r' @= glyphUpRes; 'R' @= glyphDownRes
      'n' @= glyphUpNes; 'N' @= glyphDownNes
      'm' @= glyphUpMes; 'M' @= glyphDownMes
      'y' @= glyphUpYes; 'Y' @= glyphDownYes
      'h' @= glyphUpHes; 'H' @= glyphDownHes
      'w' @= glyphUpTransphone; 'W' @= glyphDownTransphone
      'a' @= glyphUpAt; 'A' @= glyphDownAt
      'á' @= glyphUpAtAcute; 'Á' @= glyphDownAtAcute
      'à' @= glyphUpAtGrave; 'À' @= glyphDownAtGrave
      'â' @= glyphUpAtCircumflex; 'Â' @= glyphDownAtCircumflex
      'e' @= glyphUpEt; 'E' @= glyphDownEt
      'é' @= glyphUpEtAcute; 'É' @= glyphDownEtAcute
      'è' @= glyphUpEtGrave; 'È' @= glyphDownEtGrave
      'ê' @= glyphUpEtCircumflex; 'Ê' @= glyphDownEtCircumflex
      'i' @= glyphUpIt; 'I' @= glyphDownIt
      'í' @= glyphUpItAcute; 'Í' @= glyphDownItAcute
      'ì' @= glyphUpItGrave; 'Ì' @= glyphDownItGrave
      'î' @= glyphUpItCircumflex; 'Î' @= glyphDownItCircumflex
      'o' @= glyphUpOt; 'O' @= glyphDownOt
      'ò' @= glyphUpOtGrave; 'Ò' @= glyphDownOtGrave
      'ô' @= glyphUpOtCircumflex; 'Ô' @= glyphDownOtCircumflex
      'u' @= glyphUpUt; 'U' @= glyphDownUt
      'ù' @= glyphUpUtGrave; 'Ù' @= glyphDownUtGrave
      'û' @= glyphUpUtCircumflex; 'Û' @= glyphDownUtCircumflex
      ',' @= glyphTadek
      '.' @= glyphDek
      '!' @= glyphDek
      '?' @= glyphDek
      '\'' @= glyphNok
      'ʻ' @= glyphNok
      ' ' @= glyphSpace

glyphUpSal :: Given Config => Glyph
glyphUpSal = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase
      partLeftShortOblique
      partRightShortOblique
      partDiamond

glyphDownSal :: Given Config => Glyph
glyphDownSal = glyphByWith' singleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightOblique # transpose

glyphUpZol :: Given Config => Glyph
glyphUpZol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpSal
      partTransphone

glyphDownZol :: Given Config => Glyph
glyphDownZol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownSal
      partTransphone # transpose

glyphUpTal :: Given Config => Glyph
glyphUpTal = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique

glyphDownTal :: Given Config => Glyph
glyphDownTal = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose

glyphUpDol :: Given Config => Glyph
glyphUpDol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpTal
      partTransphone

glyphDownDol :: Given Config => Glyph
glyphDownDol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownTal
      partTransphone # transpose

glyphUpKal :: Given Config => Glyph
glyphUpKal = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partLeftAscender

glyphDownKal :: Given Config => Glyph
glyphDownKal = glyphByWith' singleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightChippedDescender # transpose

glyphUpGol :: Given Config => Glyph
glyphUpGol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpKal
      partTransphone

glyphDownGol :: Given Config => Glyph
glyphDownGol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownKal
      partTransphone # transpose

glyphUpFal :: Given Config => Glyph
glyphUpFal = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique

glyphDownFal :: Given Config => Glyph
glyphDownFal = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partRightOblique # transpose

glyphUpVol :: Given Config => Glyph
glyphUpVol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpFal
      partTransphone

glyphDownVol :: Given Config => Glyph
glyphDownVol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownFal
      partTransphone # transpose

glyphUpPal :: Given Config => Glyph
glyphUpPal = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique
      partRightAscender

glyphDownPal :: Given Config => Glyph
glyphDownPal = glyphByWith' singleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftChippedOblique # transpose
      partRightOblique # transpose
      partLeftChippedDescender # transpose

glyphUpBol :: Given Config => Glyph
glyphUpBol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpPal
      partTransphone

glyphDownBol :: Given Config => Glyph
glyphDownBol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownPal
      partTransphone # transpose

glyphUpCal :: Given Config => Glyph
glyphUpCal = glyphByWith' singleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftChippedOblique
      partRightOblique
      partLeftChippedDescender

glyphDownCal :: Given Config => Glyph
glyphDownCal = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partRightAscender # transpose

glyphUpQol :: Given Config => Glyph
glyphUpQol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpCal
      partTransphone

glyphDownQol :: Given Config => Glyph
glyphDownQol = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownCal
      partTransphone # transpose

glyphUpXal :: Given Config => Glyph
glyphUpXal = glyphByWith' doubleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftCutOblique
      partRightCenterOblique
      partChippedBase # transpose #~> (triangleWidth / 2 &| 0)
      partRightCutOblique # transpose #~> (triangleWidth / 2 &| 0)

glyphDownXal :: Given Config => Glyph
glyphDownXal = glyphByWith' doubleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partLeftCutOblique # transpose
      partRightCenterOblique # transpose
      partChippedBase #~> (triangleWidth / 2 &| 0)
      partRightCutOblique #~> (triangleWidth / 2 &| 0)

glyphUpJol :: Given Config => Glyph
glyphUpJol = glyphByWith' doubleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpXal
      partTransphone # transpose #~> (triangleWidth / 2 &| 0)

glyphDownJol :: Given Config => Glyph
glyphDownJol = glyphByWith' doubleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownXal
      partTransphone #~> (triangleWidth / 2 &| 0)

glyphUpLes :: Given Config => Glyph
glyphUpLes = glyphByWith' singleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftOblique
      partRightChippedOblique
      partRightChippedDescender

glyphDownLes :: Given Config => Glyph
glyphDownLes = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose
      partLeftAscender # transpose

glyphUpRes :: Given Config => Glyph
glyphUpRes = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpLes
      partTransphone

glyphDownRes :: Given Config => Glyph
glyphDownRes = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownLes
      partTransphone # transpose

glyphUpNes :: Given Config => Glyph
glyphUpNes = glyphByWith' doubleSpacing parts
  where
    parts = do
      partBase
      partRightCenterOblique
      partBase # transpose #~> (triangleWidth / 2 &| 0)

glyphDownNes :: Given Config => Glyph
glyphDownNes = glyphByWith' doubleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightOblique #~> (triangleWidth / 2 &| 0)

glyphUpMes :: Given Config => Glyph
glyphUpMes = glyphByWith' doubleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpNes
      partTransphone # transpose #~> (triangleWidth / 2 &| 0)

glyphDownMes :: Given Config => Glyph
glyphDownMes = glyphByWith' doubleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownNes
      partTransphone #~> (triangleWidth / 2 &| 0)

glyphUpYes :: Given Config => Glyph
glyphUpYes = glyphByWith' singleSpacing parts
  where
    parts = do
      partLeftOblique
      partRightOblique

glyphDownYes :: Given Config => Glyph
glyphDownYes = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftShortOblique # transpose
      partRightShortOblique # transpose
      partDiamond # transpose

glyphUpHes :: Given Config => Glyph
glyphUpHes = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphUpYes
      partTransphone

glyphDownHes :: Given Config => Glyph
glyphDownHes = glyphByWith' singleTransphoneSpacing parts
  where
    parts = do
      partIn glyphDownYes
      partTransphone # transpose

glyphUpTransphone :: Given Config => Glyph
glyphUpTransphone = glyphByWith' spacing parts
  where
    parts = do
      partTransphone # transpose #~> (-triangleWidth / 2 - horizontalTransphoneGap - thickness / sinA obliqueAngle &| 0)
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2

glyphDownTransphone :: Given Config => Glyph
glyphDownTransphone = glyphByWith' spacing parts
  where
    parts = do
      partTransphone #~> (-triangleWidth / 2 - horizontalTransphoneGap - thickness / sinA obliqueAngle &| 0)
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2

glyphUpAt :: Given Config => Glyph
glyphUpAt = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase
      partLeftOblique
      partRightOblique

glyphDownAt :: Given Config => Glyph
glyphDownAt = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightOblique # transpose

glyphUpAtAcute :: Given Config => Glyph
glyphUpAtAcute = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpAt
      partUpperAcute

glyphDownAtAcute :: Given Config => Glyph
glyphDownAtAcute = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownAt
      partLowerAcute # transpose
      
glyphUpAtGrave :: Given Config => Glyph
glyphUpAtGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpAt
      partUpperGrave

glyphDownAtGrave :: Given Config => Glyph
glyphDownAtGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownAt
      partLowerGrave # transpose

glyphUpAtCircumflex :: Given Config => Glyph
glyphUpAtCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpAt
      partUpperCircumflex

glyphDownAtCircumflex :: Given Config => Glyph
glyphDownAtCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownAt
      partLowerCircumflex # transpose

glyphUpEt :: Given Config => Glyph
glyphUpEt = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partLeftAscender

glyphDownEt :: Given Config => Glyph
glyphDownEt = glyphByWith' singleSpacing parts
  where
    parts = do
      partLeftOblique # transpose
      partRightChippedOblique # transpose
      partRightChippedDescender # transpose

glyphUpEtAcute :: Given Config => Glyph
glyphUpEtAcute = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpEt
      partLowerAcute

glyphDownEtAcute :: Given Config => Glyph
glyphDownEtAcute = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownEt
      partUpperAcute # transpose

glyphUpEtGrave :: Given Config => Glyph
glyphUpEtGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpEt
      partLowerGrave

glyphDownEtGrave :: Given Config => Glyph
glyphDownEtGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownEt
      partUpperGrave # transpose

glyphUpEtCircumflex :: Given Config => Glyph
glyphUpEtCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpEt
      partLowerCircumflex

glyphDownEtCircumflex :: Given Config => Glyph
glyphDownEtCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownEt
      partUpperCircumflex # transpose

glyphUpIt :: Given Config => Glyph
glyphUpIt = glyphByWith' singleSpacing parts
  where
    parts = do
      partLeftChippedOblique
      partRightOblique
      partLeftChippedDescender

glyphDownIt :: Given Config => Glyph
glyphDownIt = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partRightAscender # transpose

glyphUpItAcute :: Given Config => Glyph
glyphUpItAcute = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpIt
      partUpperAcute

glyphDownItAcute :: Given Config => Glyph
glyphDownItAcute = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownIt
      partLowerAcute # transpose

glyphUpItGrave :: Given Config => Glyph
glyphUpItGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpIt
      partUpperGrave

glyphDownItGrave :: Given Config => Glyph
glyphDownItGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownIt
      partLowerGrave # transpose

glyphUpItCircumflex :: Given Config => Glyph
glyphUpItCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpIt
      partUpperCircumflex

glyphDownItCircumflex :: Given Config => Glyph
glyphDownItCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownIt
      partLowerCircumflex # transpose

glyphUpOt :: Given Config => Glyph
glyphUpOt = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase
      partRightOblique
      partRightAscender

glyphDownOt :: Given Config => Glyph
glyphDownOt = glyphByWith' singleSpacing parts
  where
    parts = do
      partChippedBase # transpose
      partRightOblique # transpose
      partLeftDescender # transpose

glyphUpOtGrave :: Given Config => Glyph
glyphUpOtGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpOt
      partLowerGrave

glyphDownOtGrave :: Given Config => Glyph
glyphDownOtGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownOt
      partUpperGrave # transpose

glyphUpOtCircumflex :: Given Config => Glyph
glyphUpOtCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpOt
      partLowerCircumflex

glyphDownOtCircumflex :: Given Config => Glyph
glyphDownOtCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownOt
      partUpperCircumflex # transpose

glyphUpUt :: Given Config => Glyph
glyphUpUt = glyphByWith' singleSpacing parts
  where
    parts = do
      partChippedBase
      partLeftOblique
      partRightDescender

glyphDownUt :: Given Config => Glyph
glyphDownUt = glyphByWith' singleSpacing parts
  where
    parts = do
      partBase # transpose
      partLeftOblique # transpose
      partLeftAscender # transpose

glyphUpUtGrave :: Given Config => Glyph
glyphUpUtGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpUt
      partUpperGrave

glyphDownUtGrave :: Given Config => Glyph
glyphDownUtGrave = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownUt
      partLowerGrave # transpose

glyphUpUtCircumflex :: Given Config => Glyph
glyphUpUtCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphUpUt
      partUpperCircumflex

glyphDownUtCircumflex :: Given Config => Glyph
glyphDownUtCircumflex = glyphByWith' singleSpacing parts
  where
    parts = do
      partIn glyphDownUt
      partLowerCircumflex # transpose

glyphTadek :: Given Config => Glyph
glyphTadek = glyphByWith' spacing parts
  where
    parts = do
      partDot
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2

glyphDek :: Given Config => Glyph
glyphDek = glyphByWith' spacing parts
  where
    parts = do
      partDot
      partDot #~> (thickness / sinA obliqueAngle &| 0)
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2 + thickness / sinA obliqueAngle

glyphNok :: Given Config => Glyph
glyphNok = glyphByWith' spacing parts
  where
    parts = do
      partDot
    spacing = defaultSpacing &~ do
      fixedWidth += triangleWidth / 2

glyphSpace :: Given Config => Glyph
glyphSpace = glyphByWith' spacing skip
  where
    spacing = with &~ do
      fixedWidth .= spaceWidth