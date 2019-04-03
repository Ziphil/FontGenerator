--


module Ziphil.Font.Vekos.Glyph
  ( correspondence
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Prelude
import Ziphil.Font.Util
import Ziphil.Font.Vekos.Param
import Ziphil.Font.Vekos.Part
import Ziphil.Util.Core


correspondence :: Map Char Glyph
correspondence = Map.fromList list
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
      , ('a', glyphAt)
      , (' ', glyphSpace)
      ]

glyphLes :: Glyph
glyphLes = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphRes :: Glyph
glyphRes = makeGlyph bearing bearing $ mconcat parts
  where
    parts =
      [ partLes # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphPal :: Glyph
glyphPal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # rotateHalfTurn # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphBol :: Glyph
glyphBol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # rotateHalfTurn # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphCal :: Glyph
glyphCal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # reflectX # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphQol :: Glyph
glyphQol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # reflectX # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphKal :: Glyph
glyphKal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphGol :: Glyph
glyphGol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphYes :: Glyph
glyphYes = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partYes # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphHes :: Glyph
glyphHes = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partYes # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphSal :: Glyph
glyphSal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partYes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphZol :: Glyph
glyphZol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partYes # reflectY # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneGap, 0)
      ]

glyphTal :: Glyph
glyphTal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partTal # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphDol :: Glyph
glyphDol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partTal # translate ~^ (talWidth / 2, mean / 2)
      , partTransphone # translate ~^ (talWidth + transphoneGap, 0)
      ]

glyphFal :: Glyph
glyphFal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partTal # reflectX # translate ~^ (talWidth / 2, mean / 2)
      ]

glyphVol :: Glyph
glyphVol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partTal # reflectX # translate ~^ (talWidth / 2, mean / 2)
      , partTransphone # translate ~^ (talWidth + transphoneGap, 0)
      ]
  
glyphXal :: Glyph
glyphXal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partNarrowBowl # translate ~^ (narrowBowlVirtualWidth / 2, mean / 2)
      , partNarrowBowl # reflectX # reversePath # translate ~^ (narrowBowlVirtualWidth + narrowBowlVirtualWidth / 2 - weightX, mean / 2)
      ]

glyphJol :: Glyph
glyphJol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partNarrowBowl # translate ~^ (narrowBowlVirtualWidth / 2, mean / 2)
      , partNarrowBowl # reflectX # reversePath # translate ~^ (narrowBowlVirtualWidth / 2 * 3 - weightX, mean / 2)
      , partTransphone # translate ~^ (narrowBowlVirtualWidth * 2 - weightX + transphoneGap, 0)
      ]

glyphAt :: Glyph
glyphAt = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphSpace :: Glyph
glyphSpace = makeGlyph 0 spaceWidth $ mempty

makeGlyph :: Double -> Double -> Part -> Glyph
makeGlyph left right = addBearing left right . fixVertical descent (mean * 2) . strokePath