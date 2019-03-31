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
      [ ('l', glyphLes)
      , ('r', glyphRes)
      , ('p', glyphPal)
      , ('b', glyphBol)
      , ('c', glyphCal)
      , ('q', glyphQol)
      , ('k', glyphKal)
      , ('g', glyphGol)
      , ('y', glyphYes)
      , ('h', glyphHes)
      , ('s', glyphSal)
      , ('z', glyphZol)
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
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
      ]

glyphPal :: Glyph
glyphPal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # transTurn # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphBol :: Glyph
glyphBol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # transTurn # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
      ]

glyphCal :: Glyph
glyphCal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # transReverse # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphQol :: Glyph
glyphQol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # transReverse # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
      ]

glyphKal :: Glyph
glyphKal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # transInvert # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphGol :: Glyph
glyphGol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # transInvert # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
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
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
      ]

glyphSal :: Glyph
glyphSal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partYes # transInvert # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphZol :: Glyph
glyphZol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partYes # transInvert # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
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
makeGlyph left right = addBearing left right . fixVertical descender (mean * 2) . strokePath