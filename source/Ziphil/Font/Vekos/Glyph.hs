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
      ]

glyphLes :: Glyph
glyphLes = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # translate ~: r2 (0, mean / 2 + descender)
      ]

glyphRes :: Glyph
glyphRes = makeGlyph bearing bearing $ mconcat parts
  where
    parts =
      [ partLes # translate ~: r2 (0, mean / 2 + descender)
      , partTransphone # translate ~: r2 (bowlWidth + transphoneSpace, mean + descender)
      ]

glyphPal :: Glyph
glyphPal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # turn # translate ~: r2 (bowlWidth, mean / 2 + descender)
      ]

glyphBol :: Glyph
glyphBol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # turn # translate ~: r2 (bowlWidth, mean / 2 + descender)
      , partTransphone # translate ~: r2 (bowlWidth + transphoneSpace, mean + descender)
      ]

glyphCal :: Glyph
glyphCal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # reverse # translate ~: r2 (bowlWidth, mean / 2 + descender)
      ]

glyphQol :: Glyph
glyphQol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # reverse # translate ~: r2 (bowlWidth, mean / 2 + descender)
      , partTransphone # translate ~: r2 (bowlWidth + transphoneSpace, mean + descender)
      ]

glyphKal :: Glyph
glyphKal = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # invert # translate ~: r2 (0, mean / 2 + descender)
      ]

glyphGol :: Glyph
glyphGol = makeGlyph bearing bearing $ mconcat parts
  where
    parts = 
      [ partLes # invert # translate ~: r2 (0, mean / 2 + descender)
      , partTransphone # translate ~: r2 (bowlWidth + transphoneSpace, mean + descender)
      ]

makeGlyph :: Double -> Double -> Part -> Glyph
makeGlyph left right = addBearing left right . fixVertical (mean + descender * 2) . strokePath