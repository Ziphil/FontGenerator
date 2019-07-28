{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Glyph
  ( glyphs
  )
where

import Data.FontGen
import Ziphil.FontGen.Vekos.Config
import Ziphil.FontGen.Vekos.GlyphUtil
import Ziphil.FontGen.Vekos.Part
import Ziphil.FontGen.Vekos.Value


glyphs :: Given Config => Glyphs
glyphs = glyphsBy' list
  where
    list = do
      'l' @= glyphLes; 'r' @= glyphRes
      'p' @= glyphPal; 'b' @= glyphBol
      'c' @= glyphCal; 'q' @= glyphQol
      'k' @= glyphKal; 'g' @= glyphGol
      'y' @= glyphYes; 'h' @= glyphHes
      's' @= glyphSal; 'z' @= glyphZol
      't' @= glyphTal; 'd' @= glyphDol
      'f' @= glyphFal; 'v' @= glyphVol
      'x' @= glyphXal; 'j' @= glyphJol
      'n' @= glyphNes; 'm' @= glyphMes
      'w' @= glyphTransphone
      'a' @= glyphAt; 'á' @= glyphAtAcute; 'à' @= glyphAtGrave; 'â' @= glyphAtCircumflex
      'e' @= glyphEt; 'é' @= glyphEtAcute; 'è' @= glyphEtGrave; 'ê' @= glyphEtCircumflex
      'i' @= glyphIt; 'í' @= glyphItAcute; 'ì' @= glyphItGrave; 'î' @= glyphItCircumflex
      'u' @= glyphUt; 'ù' @= glyphUtGrave; 'û' @= glyphUtCircumflex
      'o' @= glyphOt; 'ò' @= glyphOtGrave; 'ô' @= glyphOtCircumflex
      '6' @= glyphRac; '4' @= glyphPav; '2' @= glyphQic; '8' @= glyphKeq
      '0' @= glyphNuf; '5' @= glyphXef
      '1' @= glyphTas; '9' @= glyphVun
      '3' @= glyphYus; '7' @= glyphSiz
      ',' @= glyphTadek; '.' @= glyphDek
      '!' @= glyphBadek; '?' @= glyphPadek
      '\'' @= glyphNok; 'ʻ' @= glyphDikak
      '·' @= glyphMiddot; ':' @= glyphKaltak
      '-' @= glyphFek; '…' @= glyphFohak; '—' @= glyphDash
      '[' @= glyphOpeningRakut; ']' @= glyphClosingRakut; '«' @= glyphOpeningRakut; '»' @= glyphClosingRakut
      ' ' @= glyphSpace

glyphLes :: Given Config => Glyph
glyphLes = glyphBy' parts
  where
    parts = do 
      partLes #~> (bowlWidth / 2 &| mean / 2)

glyphRes :: Given Config => Glyph
glyphRes = glyphBy' parts
  where
    parts = do
      partLes #~> (bowlWidth / 2 &| mean / 2)
      partTransphone #~> (bowlWidth + transphoneGap &| mean / 2)

glyphPal :: Given Config => Glyph
glyphPal = glyphBy' parts
  where
    parts = do 
      partLes # rotateHalfTurn #~> (bowlWidth / 2 &| mean / 2)

glyphBol :: Given Config => Glyph
glyphBol = glyphBy' parts
  where
    parts = do 
      partLes # rotateHalfTurn #~> (bowlWidth / 2 &| mean / 2)
      partTransphone #~> (bowlWidth + transphoneGap &| mean / 2)

glyphCal :: Given Config => Glyph
glyphCal = glyphBy' parts
  where
    parts = do 
      partLes # reflectX #~> (bowlWidth / 2 &| mean / 2)

glyphQol :: Given Config => Glyph
glyphQol = glyphBy' parts
  where
    parts = do 
      partLes # reflectX #~> (bowlWidth / 2 &| mean / 2)
      partTransphone #~> (bowlWidth + transphoneGap &| mean / 2)

glyphKal :: Given Config => Glyph
glyphKal = glyphBy' parts
  where
    parts = do 
      partLes # reflectY #~> (bowlWidth / 2 &| mean / 2)

glyphGol :: Given Config => Glyph
glyphGol = glyphBy' parts
  where
    parts = do 
      partLes # reflectY #~> (bowlWidth / 2 &| mean / 2)
      partTransphone #~> (bowlWidth + transphoneGap &| mean / 2)

glyphYes :: Given Config => Glyph
glyphYes = glyphBy' parts
  where
    parts = do 
      partYes #~> (bowlWidth / 2 &| mean / 2)

glyphHes :: Given Config => Glyph
glyphHes = glyphBy' parts
  where
    parts = do 
      partYes #~> (bowlWidth / 2 &| mean / 2)
      partTransphone #~> (bowlWidth + transphoneGap &| mean / 2)

glyphSal :: Given Config => Glyph
glyphSal = glyphBy' parts
  where
    parts = do 
      partYes # reflectY #~> (bowlWidth / 2 &| mean / 2)

glyphZol :: Given Config => Glyph
glyphZol = glyphBy' parts
  where
    parts = do 
      partYes # reflectY #~> (bowlWidth / 2 &| mean / 2)
      partTransphone #~> (bowlWidth + transphoneGap &| mean / 2)

glyphTal :: Given Config => Glyph
glyphTal = glyphBy' parts
  where
    parts = do 
      partTal #~> (talWidth / 2 &| mean / 2)

glyphDol :: Given Config => Glyph
glyphDol = glyphBy' parts
  where
    parts = do 
      partTal #~> (talWidth / 2 &| mean / 2)
      partTransphone #~> (talWidth + transphoneGap &| mean / 2)

glyphFal :: Given Config => Glyph
glyphFal = glyphBy' parts
  where
    parts = do 
      partTal # reflectX #~> (talWidth / 2 &| mean / 2)

glyphVol :: Given Config => Glyph
glyphVol = glyphBy' parts
  where
    parts = do 
      partTal # reflectX #~> (talWidth / 2 &| mean / 2)
      partTransphone #~> (talWidth + transphoneGap &| mean / 2)
  
glyphXal :: Given Config => Glyph
glyphXal = glyphBy' parts
  where
    parts = do 
      partXal #~> (xalWidth / 2 &| mean / 2)

glyphJol :: Given Config => Glyph
glyphJol = glyphBy' parts
  where
    parts = do 
      partXal #~> (xalWidth / 2 &| mean / 2)
      partTransphone #~> (xalWidth + transphoneGap &| mean / 2)

glyphNes :: Given Config => Glyph
glyphNes = glyphBy' parts
  where
    parts = do 
      partNes #~> (nesWidth / 2 &| mean / 2)

glyphMes :: Given Config => Glyph
glyphMes = glyphBy' parts
  where
    parts = do 
      partNes #~> (nesWidth / 2 &| mean / 2)
      partTransphone #~> (nesWidth + transphoneGap &| mean / 2)

glyphTransphone :: Given Config => Glyph
glyphTransphone = glyphByWith' spacing parts
  where
    parts = do
      partTransphone #~> (transphoneBend &| mean / 2)
    spacing = with &~ do
      leftBearing .= transphoneGap - transphoneBend - bearing
      rightBearing .= bearing

glyphAt :: Given Config => Glyph
glyphAt = glyphBy' parts
  where
    parts = do 
      partBowl #~> (bowlWidth / 2 &| mean / 2)

glyphAtAcute :: Given Config => Glyph
glyphAtAcute = glyphBy' parts
  where
    parts = do 
      partBowl #~> (bowlWidth / 2 &| mean / 2)
      partAcute #~> (bowlWidth / 2 &| mean + diacriticGap)

glyphAtGrave :: Given Config => Glyph
glyphAtGrave = glyphBy' parts
  where
    parts = do 
      partBowl #~> (bowlWidth / 2 &| mean / 2)
      partAcute # reflectY #~> (bowlWidth / 2 &| mean + acuteHeight + diacriticGap)

glyphAtCircumflex :: Given Config => Glyph
glyphAtCircumflex = glyphBy' parts
  where
    parts = do 
      partBowl #~> (bowlWidth / 2 &| mean / 2)
      partCircumflex #~> (bowlWidth / 2 &| mean + diacriticGap)

glyphIt :: Given Config => Glyph
glyphIt = glyphBy' parts
  where
    parts = do 
      partIt #~> (talWidth / 2 &| mean / 2)

glyphItAcute :: Given Config => Glyph
glyphItAcute = glyphBy' parts
  where
    parts = do 
      partIt #~> (talWidth / 2 &| mean / 2)
      partAcute #~> (bowlWidth / 2 &| mean + diacriticGap)

glyphItGrave :: Given Config => Glyph
glyphItGrave = glyphBy' parts
  where
    parts = do 
      partIt #~> (talWidth / 2 &| mean / 2)
      partAcute # reflectY #~> (bowlWidth / 2 &| mean + acuteHeight + diacriticGap)

glyphItCircumflex :: Given Config => Glyph
glyphItCircumflex = glyphBy' parts
  where
    parts = do 
      partIt #~> (talWidth / 2 &| mean / 2)
      partCircumflex #~> (bowlWidth / 2 &| mean + diacriticGap)

glyphEt :: Given Config => Glyph
glyphEt = glyphBy' parts
  where
    parts = do 
      partIt # rotateHalfTurn #~> (talWidth / 2 &| mean / 2)

glyphEtAcute :: Given Config => Glyph
glyphEtAcute = glyphBy' parts
  where
    parts = do 
      partIt # rotateHalfTurn #~> (talWidth / 2 &| mean / 2)
      partAcute # reflectY #~> (talBeakWidth &| -diacriticGap)

glyphEtGrave :: Given Config => Glyph
glyphEtGrave = glyphBy' parts
  where
    parts = do 
      partIt # rotateHalfTurn #~> (talWidth / 2 &| mean / 2)
      partAcute #~> (talBeakWidth &| -acuteHeight - diacriticGap)

glyphEtCircumflex :: Given Config => Glyph
glyphEtCircumflex = glyphBy' parts
  where
    parts = do 
      partIt # rotateHalfTurn #~> (talWidth / 2 &| mean / 2)
      partCircumflex #~> (talBeakWidth &| -circumflexHeight - diacriticGap)

glyphUt :: Given Config => Glyph
glyphUt = glyphBy' parts
  where
    parts = do 
      partUt #~> (talWidth / 2 &| mean / 2)

glyphUtGrave :: Given Config => Glyph
glyphUtGrave = glyphBy' parts
  where
    parts = do 
      partUt #~> (talWidth / 2 &| mean / 2)
      partAcute # reflectY #~> (bowlWidth / 2 &| mean + acuteHeight + diacriticGap)

glyphUtCircumflex :: Given Config => Glyph
glyphUtCircumflex = glyphBy' parts
  where
    parts = do 
      partUt #~> (talWidth / 2 &| mean / 2)
      partCircumflex #~> (bowlWidth / 2 &| mean + diacriticGap)

glyphOt :: Given Config => Glyph
glyphOt = glyphBy' parts
  where
    parts = do 
      partUt # rotateHalfTurn #~> (talWidth / 2 &| mean / 2)

glyphOtGrave :: Given Config => Glyph
glyphOtGrave = glyphBy' parts
  where
    parts = do 
      partUt # rotateHalfTurn #~> (talWidth / 2 &| mean / 2)
      partAcute #~> (talBeakWidth &| -acuteHeight - diacriticGap)

glyphOtCircumflex :: Given Config => Glyph
glyphOtCircumflex = glyphBy' parts
  where
    parts = do 
      partUt # rotateHalfTurn #~> (talWidth / 2 &| mean / 2)
      partCircumflex #~> (talBeakWidth &| -circumflexHeight - diacriticGap)

glyphRac :: Given Config => Glyph
glyphRac = glyphBy' parts
  where
    parts = do
      partRac #~> (bowlWidth / 2 &| mean / 2)

glyphPav :: Given Config => Glyph
glyphPav = glyphBy' parts
  where
    parts = do
      partRac # rotateHalfTurn #~> (bowlWidth / 2 &| mean / 2)

glyphQic :: Given Config => Glyph
glyphQic = glyphBy' parts
  where
    parts = do
      partRac # reflectX #~> (bowlWidth / 2 &| mean / 2)

glyphKeq :: Given Config => Glyph
glyphKeq = glyphBy' parts
  where
    parts = do
      partRac # reflectY #~> (bowlWidth / 2 &| mean / 2)
  
glyphNuf :: Given Config => Glyph
glyphNuf = glyphBy' parts
  where
    parts = do
      partNuf #~> (bowlWidth / 2 &| mean / 2)

glyphXef :: Given Config => Glyph
glyphXef = glyphBy' parts
  where
    parts = do 
      partXef #~> (xefWidth / 2 &| mean / 2)

glyphTas :: Given Config => Glyph
glyphTas = glyphBy' parts
  where
    parts = do
      partTas #~> (tasWidth / 2 &| mean / 2)

glyphVun :: Given Config => Glyph
glyphVun = glyphBy' parts
  where
    parts = do
      partTas # rotateHalfTurn #~> (tasWidth / 2 &| mean / 2)

glyphYus :: Given Config => Glyph
glyphYus = glyphBy' parts
  where
    parts = do
      partYus #~> (yusWidth / 2 &| mean / 2)

glyphSiz :: Given Config => Glyph
glyphSiz = glyphBy' parts
  where
    parts = do
      partYus # rotateHalfTurn #~> (yusWidth / 2 &| mean / 2)

glyphTadek :: Given Config => Glyph
glyphTadek = glyphBy' parts
  where
    parts = do
      partDot

glyphDek :: Given Config => Glyph
glyphDek = glyphBy' parts
  where
    parts = do
      partDot
      partDot #~> (dotWidth + dotGap &| 0)

glyphBadek :: Given Config => Glyph
glyphBadek = glyphByWith' spacing parts
  where
    parts = do
      partDot
      partDot #~> (dotWidth + dotGap &| 0)
      partBadekStem #~> (dotWidth / 2 - thicknessX / 2 &| dotWidth + badekGap - overshoot)
    spacing = with &~ do
      leftBearing .= badekLeftBearing
      rightBearing .= bearing

glyphPadek :: Given Config => Glyph
glyphPadek = glyphByWith' spacing parts
  where
    parts = do
      partDot
      partDot #~> (dotWidth + dotGap &| 0)
      partPadekStem #~> (dotWidth / 2 - thicknessX / 2 &| dotWidth + badekGap - overshoot)
    spacing = with &~ do
      leftBearing .= badekLeftBearing
      rightBearing .= bearing

glyphNok :: Given Config => Glyph
glyphNok = glyphBy' parts
  where
    parts = do
      partNok #~> (0 &| ascent)

glyphDikak :: Given Config => Glyph
glyphDikak = glyphByWith' spacing parts
  where
    parts = do
      partDikak #~> (dikakBend &| ascent)
    spacing = with &~ do
      leftBearing .= bearing
      rightBearing .= dikakRightBearing

glyphMiddot :: Given Config => Glyph
glyphMiddot = glyphBy' parts
  where
    parts = do
      partFloatingDot #~> (0 &| middotAltitude)

glyphKaltak :: Given Config => Glyph
glyphKaltak = glyphByWith' spacing parts
  where
    parts = do
      partDot 
      partFloatingDot #~> (0 &| upperKaltakAltitude)
    spacing = with &~ do
      leftBearing .= kaltakBearing
      rightBearing .= kaltakBearing

glyphFek :: Given Config => Glyph
glyphFek = glyphBy' parts
  where
    parts = do
      partFek #~> (0 &| fekAltitude + thicknessY / 2)

glyphFohak :: Given Config => Glyph
glyphFohak = glyphBy' parts
  where
    parts = do
      partFohak #~> (0 &| thicknessY)

glyphDash :: Given Config => Glyph
glyphDash = glyphBy' parts
  where
    parts = do
      partDash #~> (0 &| dashAltitude + thicknessY / 2)

glyphOpeningRakut :: Given Config => Glyph
glyphOpeningRakut = glyphBy' parts
  where
    parts = do
      partOpeningRakut #~> (0 &| ascent)

glyphClosingRakut :: Given Config => Glyph
glyphClosingRakut = glyphBy' parts
  where
    parts = do
      partOpeningRakut # reflectX #~> (rakutWidth &| ascent)
      
glyphSpace :: Given Config => Glyph
glyphSpace = glyphByWith' spacing skip
  where
    spacing = with &~ do
      leftBearing .= spaceWidth