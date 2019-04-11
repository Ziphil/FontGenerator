{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Glyph
  ( glyphs
  )
where

import Data.FontGen
import qualified Data.Map as Map
import Data.Reflection
import Ziphil.FontGen.Vekos.Param
import Ziphil.FontGen.Vekos.Part


glyphs :: Given Config => Glyphs
glyphs = Map.fromList list
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
      , ('a', glyphAt), ('á', glyphAtAcute), ('à', glyphAtGrave), ('â', glyphAtCircumflex)
      , ('e', glyphEt), ('é', glyphEtAcute), ('è', glyphEtGrave), ('ê', glyphEtCircumflex)
      , ('i', glyphIt), ('í', glyphItAcute), ('ì', glyphItGrave), ('î', glyphItCircumflex)
      , ('u', glyphUt), ('ù', glyphUtGrave), ('û', glyphUtCircumflex)
      , ('o', glyphOt), ('ò', glyphOtGrave), ('ô', glyphOtCircumflex)
      , (' ', glyphSpace)
      ]

glyphLes :: Given Config => Glyph
glyphLes = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partLes # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphRes :: Given Config => Glyph
glyphRes = makeGlyph' bearing bearing parts
  where
    parts =
      [ partLes # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphPal :: Given Config => Glyph
glyphPal = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partLes # rotateHalfTurn # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphBol :: Given Config => Glyph
glyphBol = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partLes # rotateHalfTurn # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphCal :: Given Config => Glyph
glyphCal = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partLes # reflectX # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphQol :: Given Config => Glyph
glyphQol = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partLes # reflectX # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphKal :: Given Config => Glyph
glyphKal = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partLes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphGol :: Given Config => Glyph
glyphGol = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partLes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphYes :: Given Config => Glyph
glyphYes = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partYes # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphHes :: Given Config => Glyph
glyphHes = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partYes # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphSal :: Given Config => Glyph
glyphSal = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partYes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphZol :: Given Config => Glyph
glyphZol = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partYes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphTal :: Given Config => Glyph
glyphTal = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partTal # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphDol :: Given Config => Glyph
glyphDol = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partTal # translate ~^ (talWidth / 2, mean / 2)
      , partTransphone # translate ~^ (talWidth + transphoneGap, 0)
      ]

glyphFal :: Given Config => Glyph
glyphFal = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partTal # reflectX # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphVol :: Given Config => Glyph
glyphVol = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partTal # reflectX # translate ~^ (talWidth / 2, mean / 2)
      , partTransphone # translate ~^ (talWidth + transphoneGap, 0)
      ]
  
glyphXal :: Given Config => Glyph
glyphXal = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partNarrowBowl # translate ~^ (narrowBowlVirtualWidth / 2, mean / 2)
      , partNarrowBowl # reflectX # reversePath # translate ~^ (narrowBowlVirtualWidth / 2 * 3 - weightX, mean / 2)
      ]

glyphJol :: Given Config => Glyph
glyphJol = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partNarrowBowl # translate ~^ (narrowBowlVirtualWidth / 2, mean / 2)
      , partNarrowBowl # reflectX # reversePath # translate ~^ (narrowBowlVirtualWidth / 2 * 3 - weightX, mean / 2)
      , partTransphone # translate ~^ (narrowBowlVirtualWidth * 2 - weightX + transphoneGap, 0)
      ]

glyphAt :: Given Config => Glyph
glyphAt = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphAtAcute :: Given Config => Glyph
glyphAtAcute = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      , partAcute # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphAtGrave :: Given Config => Glyph
glyphAtGrave = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      , partAcute # reflectY # translate ~^ (bowlWidth / 2, mean + acuteHeight + diacriticGap)
      ]

glyphAtCircumflex :: Given Config => Glyph
glyphAtCircumflex = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphIt :: Given Config => Glyph
glyphIt = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partIt # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphItAcute :: Given Config => Glyph
glyphItAcute = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partIt # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphItGrave :: Given Config => Glyph
glyphItGrave = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partIt # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # reflectY # translate ~^ (bowlWidth / 2, mean + acuteHeight + diacriticGap)
      ]

glyphItCircumflex :: Given Config => Glyph
glyphItCircumflex = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partIt # translate ~^ (talWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphEt :: Given Config => Glyph
glyphEt = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphEtAcute :: Given Config => Glyph
glyphEtAcute = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # reflectY # translate ~^ (beakWidth, -diacriticGap)
      ]

glyphEtGrave :: Given Config => Glyph
glyphEtGrave = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # translate ~^ (beakWidth, -acuteHeight - diacriticGap)
      ]

glyphEtCircumflex :: Given Config => Glyph
glyphEtCircumflex = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (beakWidth, -circumflexHeight - diacriticGap)
      ]

glyphUt :: Given Config => Glyph
glyphUt = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partUt # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphUtGrave :: Given Config => Glyph
glyphUtGrave = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partUt # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # reflectY # translate ~^ (bowlWidth / 2, mean + acuteHeight + diacriticGap)
      ]

glyphUtCircumflex :: Given Config => Glyph
glyphUtCircumflex = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partUt # translate ~^ (talWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphOt :: Given Config => Glyph
glyphOt = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphOtGrave :: Given Config => Glyph
glyphOtGrave = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # translate ~^ (beakWidth, -circumflexHeight - diacriticGap)
      ]

glyphOtCircumflex :: Given Config => Glyph
glyphOtCircumflex = makeGlyph' bearing bearing parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (beakWidth, -circumflexHeight - diacriticGap)
      ]
      
glyphSpace :: Given Config => Glyph
glyphSpace = makeGlyph' 0 spaceWidth []

makeGlyph' :: Given Config => Double -> Double -> [Part] -> Glyph
makeGlyph' = makeGlyph em descent