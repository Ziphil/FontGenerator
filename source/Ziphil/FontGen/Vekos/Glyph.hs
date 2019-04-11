--


module Ziphil.FontGen.Vekos.Glyph
  ( glyphs
  )
where

import Data.FontGen
import Data.Map (Map)
import qualified Data.Map as Map
import Ziphil.FontGen.Vekos.Param
import Ziphil.FontGen.Vekos.Part


glyphs :: Glyphs
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

glyphLes :: Glyph
glyphLes = makeGlyph bearing bearing parts
  where
    parts = 
      [ partLes # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphRes :: Glyph
glyphRes = makeGlyph bearing bearing parts
  where
    parts =
      [ partLes # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphPal :: Glyph
glyphPal = makeGlyph bearing bearing parts
  where
    parts = 
      [ partLes # rotateHalfTurn # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphBol :: Glyph
glyphBol = makeGlyph bearing bearing parts
  where
    parts = 
      [ partLes # rotateHalfTurn # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphCal :: Glyph
glyphCal = makeGlyph bearing bearing parts
  where
    parts = 
      [ partLes # reflectX # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphQol :: Glyph
glyphQol = makeGlyph bearing bearing parts
  where
    parts = 
      [ partLes # reflectX # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphKal :: Glyph
glyphKal = makeGlyph bearing bearing parts
  where
    parts = 
      [ partLes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphGol :: Glyph
glyphGol = makeGlyph bearing bearing parts
  where
    parts = 
      [ partLes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphYes :: Glyph
glyphYes = makeGlyph bearing bearing parts
  where
    parts = 
      [ partYes # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphHes :: Glyph
glyphHes = makeGlyph bearing bearing parts
  where
    parts = 
      [ partYes # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphSal :: Glyph
glyphSal = makeGlyph bearing bearing parts
  where
    parts = 
      [ partYes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphZol :: Glyph
glyphZol = makeGlyph bearing bearing parts
  where
    parts = 
      [ partYes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphTal :: Glyph
glyphTal = makeGlyph bearing bearing parts
  where
    parts = 
      [ partTal # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphDol :: Glyph
glyphDol = makeGlyph bearing bearing parts
  where
    parts = 
      [ partTal # translate ~^ (talWidth / 2, mean / 2)
      , partTransphone # translate ~^ (talWidth + transphoneGap, 0)
      ]

glyphFal :: Glyph
glyphFal = makeGlyph bearing bearing parts
  where
    parts = 
      [ partTal # reflectX # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphVol :: Glyph
glyphVol = makeGlyph bearing bearing parts
  where
    parts = 
      [ partTal # reflectX # translate ~^ (talWidth / 2, mean / 2)
      , partTransphone # translate ~^ (talWidth + transphoneGap, 0)
      ]
  
glyphXal :: Glyph
glyphXal = makeGlyph bearing bearing parts
  where
    parts = 
      [ partNarrowBowl # translate ~^ (narrowBowlVirtualWidth / 2, mean / 2)
      , partNarrowBowl # reflectX # reversePath # translate ~^ (narrowBowlVirtualWidth / 2 * 3 - weightX, mean / 2)
      ]

glyphJol :: Glyph
glyphJol = makeGlyph bearing bearing parts
  where
    parts = 
      [ partNarrowBowl # translate ~^ (narrowBowlVirtualWidth / 2, mean / 2)
      , partNarrowBowl # reflectX # reversePath # translate ~^ (narrowBowlVirtualWidth / 2 * 3 - weightX, mean / 2)
      , partTransphone # translate ~^ (narrowBowlVirtualWidth * 2 - weightX + transphoneGap, 0)
      ]

glyphAt :: Glyph
glyphAt = makeGlyph bearing bearing parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphAtAcute :: Glyph
glyphAtAcute = makeGlyph bearing bearing parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      , partAcute # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphAtGrave :: Glyph
glyphAtGrave = makeGlyph bearing bearing parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      , partAcute # reflectY # translate ~^ (bowlWidth / 2, mean + acuteHeight + diacriticGap)
      ]

glyphAtCircumflex :: Glyph
glyphAtCircumflex = makeGlyph bearing bearing parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphIt :: Glyph
glyphIt = makeGlyph bearing bearing parts
  where
    parts = 
      [ partIt # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphItAcute :: Glyph
glyphItAcute = makeGlyph bearing bearing parts
  where
    parts = 
      [ partIt # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphItGrave :: Glyph
glyphItGrave = makeGlyph bearing bearing parts
  where
    parts = 
      [ partIt # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # reflectY # translate ~^ (bowlWidth / 2, mean + acuteHeight + diacriticGap)
      ]

glyphItCircumflex :: Glyph
glyphItCircumflex = makeGlyph bearing bearing parts
  where
    parts = 
      [ partIt # translate ~^ (talWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphEt :: Glyph
glyphEt = makeGlyph bearing bearing parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphEtAcute :: Glyph
glyphEtAcute = makeGlyph bearing bearing parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # reflectY # translate ~^ (beakWidth, -diacriticGap)
      ]

glyphEtGrave :: Glyph
glyphEtGrave = makeGlyph bearing bearing parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # translate ~^ (beakWidth, -acuteHeight - diacriticGap)
      ]

glyphEtCircumflex :: Glyph
glyphEtCircumflex = makeGlyph bearing bearing parts
  where
    parts = 
      [ partIt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (beakWidth, -circumflexHeight - diacriticGap)
      ]

glyphUt :: Glyph
glyphUt = makeGlyph bearing bearing parts
  where
    parts = 
      [ partUt # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphUtGrave :: Glyph
glyphUtGrave = makeGlyph bearing bearing parts
  where
    parts = 
      [ partUt # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # reflectY # translate ~^ (bowlWidth / 2, mean + acuteHeight + diacriticGap)
      ]

glyphUtCircumflex :: Glyph
glyphUtCircumflex = makeGlyph bearing bearing parts
  where
    parts = 
      [ partUt # translate ~^ (talWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (bowlWidth / 2, mean + diacriticGap)
      ]

glyphOt :: Glyph
glyphOt = makeGlyph bearing bearing parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphOtGrave :: Glyph
glyphOtGrave = makeGlyph bearing bearing parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partAcute # translate ~^ (beakWidth, -circumflexHeight - diacriticGap)
      ]

glyphOtCircumflex :: Glyph
glyphOtCircumflex = makeGlyph bearing bearing parts
  where
    parts = 
      [ partUt # rotateHalfTurn # translate ~^ (talWidth / 2, mean / 2)
      , partCircumflex # translate ~^ (beakWidth, -circumflexHeight - diacriticGap)
      ]
      
glyphSpace :: Glyph
glyphSpace = makeGlyph 0 spaceWidth []

makeGlyph :: Double -> Double -> [Part] -> Glyph
makeGlyph left right = addBearing left right . fixVertical descent (mean + descent * 2) . strokePath . mconcat