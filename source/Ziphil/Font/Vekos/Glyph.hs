--


module Ziphil.Font.Vekos.Glyph
  ( correspondence
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Prelude hiding (turn)
import Prelude hiding (reverse)
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
      , ('a', glyphAt)
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
      [ partLes # turn # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphBol :: Glyph
glyphBol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # turn # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
      ]

glyphCal :: Glyph
glyphCal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # reverse # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphQol :: Glyph
glyphQol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # reverse # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
      ]

glyphKal :: Glyph
glyphKal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # invert # translate ~^ (bowlWidth / 2, mean / 2)
      ]

glyphGol :: Glyph
glyphGol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # invert # translate ~^ (bowlWidth / 2, mean / 2)
      , partTransphone # translate ~^ (bowlWidth + transphoneSpace, 0)
      ]

glyphAt :: Glyph
glyphAt = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partBowl # translate ~^ (bowlWidth / 2, mean / 2)
      ]

makeGlyph :: Double -> Double -> Part -> Glyph
makeGlyph left right = addBearing left right . fixVertical descender (mean * 2) . strokePath