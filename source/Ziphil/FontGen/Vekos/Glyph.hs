{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Glyph
  ( glyphs
  )
where

import Data.FontGen
import Ziphil.FontGen.Vekos.Config
import Ziphil.FontGen.Vekos.Part
import Ziphil.FontGen.Vekos.Util
import Ziphil.FontGen.Vekos.Value


glyphs :: Given Config => Glyphs
glyphs = makeGlyphs list
  where
    list =
      [ ('l', glyphLes), ('r', glyphRes)
      , ('p', glyphPal), ('b', glyphBol)
      , ('c', glyphCal), ('q', glyphQol)
      , ('k', glyphKal), ('g', glyphGol)
      , ('y', glyphYes), ('h', glyphHes)
      , ('s', glyphSal), ('z', glyphZol)
      , ('t', glyphTal), ('d', glyphDol)
      , ('f', glyphFal), ('v', glyphVol)
      , ('x', glyphXal), ('j', glyphJol)
      , ('n', glyphNes), ('m', glyphMes)
      , ('a', glyphAt), ('á', glyphAtAcute), ('à', glyphAtGrave), ('â', glyphAtCircumflex)
      , ('e', glyphEt), ('é', glyphEtAcute), ('è', glyphEtGrave), ('ê', glyphEtCircumflex)
      , ('i', glyphIt), ('í', glyphItAcute), ('ì', glyphItGrave), ('î', glyphItCircumflex)
      , ('u', glyphUt), ('ù', glyphUtGrave), ('û', glyphUtCircumflex)
      , ('o', glyphOt), ('ò', glyphOtGrave), ('ô', glyphOtCircumflex)
      , ('6', glyphRac), ('4', glyphPav), ('2', glyphQic), ('8', glyphKeq)
      , ('1', glyphTas), ('9', glyphVun)
      , (',', glyphTadek), ('.', glyphDek)
      , ('!', glyphBadek), ('?', glyphPadek)
      , ('\'', glyphNok), ('ʻ', glyphDikak)
      , ('-', glyphFek)
      , ('[', glyphOpeningRakut), (']', glyphClosingRakut), ('«', glyphOpeningRakut), ('»', glyphClosingRakut)
      , (' ', glyphSpace)
      ]

glyphLes :: Given Config => Glyph
glyphLes = makeGlyph' parts
  where
    parts = 
      [ partLes # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphRes :: Given Config => Glyph
glyphRes = makeGlyph' parts
  where
    parts =
      [ partLes # translate (bowlWidth / 2 &| mean / 2)
      , partTransphone # translate (bowlWidth + transphoneGap &| mean / 2)
      ]

glyphPal :: Given Config => Glyph
glyphPal = makeGlyph' parts
  where
    parts = 
      [ partLes # rotateHalfTurn # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphBol :: Given Config => Glyph
glyphBol = makeGlyph' parts
  where
    parts = 
      [ partLes # rotateHalfTurn # translate (bowlWidth / 2 &| mean / 2)
      , partTransphone # translate (bowlWidth + transphoneGap &| mean / 2)
      ]

glyphCal :: Given Config => Glyph
glyphCal = makeGlyph' parts
  where
    parts = 
      [ partLes # reflectX # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphQol :: Given Config => Glyph
glyphQol = makeGlyph' parts
  where
    parts = 
      [ partLes # reflectX # translate (bowlWidth / 2 &| mean / 2)
      , partTransphone # translate (bowlWidth + transphoneGap &| mean / 2)
      ]

glyphKal :: Given Config => Glyph
glyphKal = makeGlyph' parts
  where
    parts = 
      [ partLes # reflectY # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphGol :: Given Config => Glyph
glyphGol = makeGlyph' parts
  where
    parts = 
      [ partLes # reflectY # translate (bowlWidth / 2 &| mean / 2)
      , partTransphone # translate (bowlWidth + transphoneGap &| mean / 2)
      ]

glyphYes :: Given Config => Glyph
glyphYes = makeGlyph' parts
  where
    parts = 
      [ partYes # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphHes :: Given Config => Glyph
glyphHes = makeGlyph' parts
  where
    parts = 
      [ partYes # translate (bowlWidth / 2 &| mean / 2)
      , partTransphone # translate (bowlWidth + transphoneGap &| mean / 2)
      ]

glyphSal :: Given Config => Glyph
glyphSal = makeGlyph' parts
  where
    parts = 
      [ partYes # reflectY # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphZol :: Given Config => Glyph
glyphZol = makeGlyph' parts
  where
    parts = 
      [ partYes # reflectY # translate (bowlWidth / 2 &| mean / 2)
      , partTransphone # translate (bowlWidth + transphoneGap &| mean / 2)
      ]

glyphTal :: Given Config => Glyph
glyphTal = makeGlyph' parts
  where
    parts = 
      [ partTal # translate (talWidth / 2 &| mean / 2)
      ]

glyphDol :: Given Config => Glyph
glyphDol = makeGlyph' parts
  where
    parts = 
      [ partTal # translate (talWidth / 2 &| mean / 2)
      , partTransphone # translate (talWidth + transphoneGap &| mean / 2)
      ]

glyphFal :: Given Config => Glyph
glyphFal = makeGlyph' parts
  where
    parts = 
      [ partTal # reflectX # translate (talWidth / 2 &| mean / 2)
      ]

glyphVol :: Given Config => Glyph
glyphVol = makeGlyph' parts
  where
    parts = 
      [ partTal # reflectX # translate (talWidth / 2 &| mean / 2)
      , partTransphone # translate (talWidth + transphoneGap &| mean / 2)
      ]
  
glyphXal :: Given Config => Glyph
glyphXal = makeGlyph' parts
  where
    parts = 
      [ partXal # translate (xalWidth / 2 &| mean / 2)
      ]

glyphJol :: Given Config => Glyph
glyphJol = makeGlyph' parts
  where
    parts = 
      [ partXal # translate (xalWidth / 2 &| mean / 2)
      , partTransphone # translate (xalWidth + transphoneGap &| mean / 2)
      ]

glyphNes :: Given Config => Glyph
glyphNes = makeGlyph' parts
  where
    parts = 
      [ partNes # translate (nesWidth / 2 &| mean / 2)
      ]

glyphMes :: Given Config => Glyph
glyphMes = makeGlyph' parts
  where
    parts = 
      [ partNes # translate (nesWidth / 2 &| mean / 2)
      , partTransphone # translate (nesWidth + transphoneGap &| mean / 2)
      ]

glyphAt :: Given Config => Glyph
glyphAt = makeGlyph' parts
  where
    parts = 
      [ partBowl # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphAtAcute :: Given Config => Glyph
glyphAtAcute = makeGlyph' parts
  where
    parts = 
      [ partBowl # translate (bowlWidth / 2 &| mean / 2)
      , partAcute # translate (bowlWidth / 2 &| mean + diacriticGap)
      ]

glyphAtGrave :: Given Config => Glyph
glyphAtGrave = makeGlyph' parts
  where
    parts = 
      [ partBowl # translate (bowlWidth / 2 &| mean / 2)
      , partAcute # reflectY # translate (bowlWidth / 2 &| mean + acuteHeight + diacriticGap)
      ]

glyphAtCircumflex :: Given Config => Glyph
glyphAtCircumflex = makeGlyph' parts
  where
    parts = 
      [ partBowl # translate (bowlWidth / 2 &| mean / 2)
      , partCircumflex # translate (bowlWidth / 2 &| mean + diacriticGap)
      ]

glyphIt :: Given Config => Glyph
glyphIt = makeGlyph' parts
  where
    parts = 
      [ partIt # translate (talWidth / 2 &| mean / 2)
      ]

glyphItAcute :: Given Config => Glyph
glyphItAcute = makeGlyph' parts
  where
    parts = 
      [ partIt # translate (talWidth / 2 &| mean / 2)
      , partAcute # translate (bowlWidth / 2 &| mean + diacriticGap)
      ]

glyphItGrave :: Given Config => Glyph
glyphItGrave = makeGlyph' parts
  where
    parts = 
      [ partIt # translate (talWidth / 2 &| mean / 2)
      , partAcute # reflectY # translate (bowlWidth / 2 &| mean + acuteHeight + diacriticGap)
      ]

glyphItCircumflex :: Given Config => Glyph
glyphItCircumflex = makeGlyph' parts
  where
    parts = 
      [ partIt # translate (talWidth / 2 &| mean / 2)
      , partCircumflex # translate (bowlWidth / 2 &| mean + diacriticGap)
      ]

glyphEt :: Given Config => Glyph
glyphEt = makeGlyph' parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate (talWidth / 2 &| mean / 2)
      ]

glyphEtAcute :: Given Config => Glyph
glyphEtAcute = makeGlyph' parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate (talWidth / 2 &| mean / 2)
      , partAcute # reflectY # translate (talBeakWidth &| -diacriticGap)
      ]

glyphEtGrave :: Given Config => Glyph
glyphEtGrave = makeGlyph' parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate (talWidth / 2 &| mean / 2)
      , partAcute # translate (talBeakWidth &| -acuteHeight - diacriticGap)
      ]

glyphEtCircumflex :: Given Config => Glyph
glyphEtCircumflex = makeGlyph' parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate (talWidth / 2 &| mean / 2)
      , partCircumflex # translate (talBeakWidth &| -circumflexHeight - diacriticGap)
      ]

glyphUt :: Given Config => Glyph
glyphUt = makeGlyph' parts
  where
    parts = 
      [ partUt # translate (talWidth / 2 &| mean / 2)
      ]

glyphUtGrave :: Given Config => Glyph
glyphUtGrave = makeGlyph' parts
  where
    parts = 
      [ partUt # translate (talWidth / 2 &| mean / 2)
      , partAcute # reflectY # translate (bowlWidth / 2 &| mean + acuteHeight + diacriticGap)
      ]

glyphUtCircumflex :: Given Config => Glyph
glyphUtCircumflex = makeGlyph' parts
  where
    parts = 
      [ partUt # translate (talWidth / 2 &| mean / 2)
      , partCircumflex # translate (bowlWidth / 2 &| mean + diacriticGap)
      ]

glyphOt :: Given Config => Glyph
glyphOt = makeGlyph' parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate (talWidth / 2 &| mean / 2)
      ]

glyphOtGrave :: Given Config => Glyph
glyphOtGrave = makeGlyph' parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate (talWidth / 2 &| mean / 2)
      , partAcute # translate (talBeakWidth &| -acuteHeight - diacriticGap)
      ]

glyphOtCircumflex :: Given Config => Glyph
glyphOtCircumflex = makeGlyph' parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate (talWidth / 2 &| mean / 2)
      , partCircumflex # translate (talBeakWidth &| -circumflexHeight - diacriticGap)
      ]

glyphRac :: Given Config => Glyph
glyphRac = makeGlyph' parts
  where
    parts =
      [ partRac # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphPav :: Given Config => Glyph
glyphPav = makeGlyph' parts
  where
    parts =
      [ partRac # rotateHalfTurn # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphQic :: Given Config => Glyph
glyphQic = makeGlyph' parts
  where
    parts =
      [ partRac # reflectX # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphKeq :: Given Config => Glyph
glyphKeq = makeGlyph' parts
  where
    parts =
      [ partRac # reflectY # translate (bowlWidth / 2 &| mean / 2)
      ]

glyphTas :: Given Config => Glyph
glyphTas = makeGlyph' parts
  where
    parts =
      [ partTas # translate (tasWidth / 2 &| mean / 2)
      ]

glyphVun :: Given Config => Glyph
glyphVun = makeGlyph' parts
  where
    parts =
      [ partTas # rotateHalfTurn # translate (tasWidth / 2 &| mean / 2)
      ]

glyphTadek :: Given Config => Glyph
glyphTadek = makeGlyph' parts
  where
    parts =
      [ partDot
      ]

glyphDek :: Given Config => Glyph
glyphDek = makeGlyph' parts
  where
    parts =
      [ partDot
      , partDot # translate (dotWidth + dotGap &| 0)
      ]

glyphBadek :: Given Config => Glyph
glyphBadek = makeGlyphWithSpacing' spacing parts
  where
    parts =
      [ partDot
      , partDot # translate (dotWidth + dotGap &| 0)
      , partBadekStem # translate (dotWidth / 2 - thicknessX / 2 &| dotWidth + badekGap - overshoot)
      ]
    spacing = with &~ do
      leftBearing .= badekLeftBearing
      rightBearing .= bearing

glyphPadek :: Given Config => Glyph
glyphPadek = makeGlyphWithSpacing' spacing parts
  where
    parts =
      [ partDot
      , partDot # translate (dotWidth + dotGap &| 0)
      , partPadekStem # translate (dotWidth / 2 - thicknessX / 2 &| dotWidth + badekGap - overshoot)
      ]
    spacing = with &~ do
      leftBearing .= badekLeftBearing
      rightBearing .= bearing

glyphNok :: Given Config => Glyph
glyphNok = makeGlyph' parts
  where
    parts =
      [ partNok # translate (0 &| ascent)
      ]

glyphDikak :: Given Config => Glyph
glyphDikak = makeGlyphWithSpacing' spacing parts
  where
    parts =
      [ partDikak # translate (dikakBend &| ascent)
      ]
    spacing = with &~ do
      leftBearing .= bearing
      rightBearing .= dikakRightBearing

glyphFek :: Given Config => Glyph
glyphFek = makeGlyph' parts
  where
    parts =
      [ partFek # translate (0 &| fekAltitude + thicknessY / 2)
      ]

glyphOpeningRakut :: Given Config => Glyph
glyphOpeningRakut = makeGlyph' parts
  where
    parts =
      [ partOpeningRakut # translate (0 &| ascent)
      ]

glyphClosingRakut :: Given Config => Glyph
glyphClosingRakut = makeGlyph' parts
  where
    parts =
      [ partOpeningRakut # reflectX # translate (rakutWidth &| ascent)
      ]
      
glyphSpace :: Given Config => Glyph
glyphSpace = makeGlyphWithSpacing' spacing []
  where
    spacing = with &~ do
      leftBearing .= spaceWidth
      rightBearing .= 0