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


correspondence :: Map Int Glyph
correspondence = Map.fromList list
  where
    list =
      [ (0x6C, glyphLes)
      , (0x72, glyphRes)
      ]

glyphLes :: Glyph
glyphLes = makeGlyph bearing bearing $ mconcat diagrams
  where
    diagrams = 
      [ translate ~: r2 (0, mean / 2 + descender) ~: stroke partBowl
      , translate ~: r2 (450 - weightX, mean / 2 + descender) ~: stroke partTail
      ]

glyphRes :: Glyph
glyphRes = makeGlyph bearing bearing $ mconcat diagrams
  where
    diagrams = undefined

makeGlyph :: Double -> Double -> Glyph -> Glyph
makeGlyph left right = addBearing left right . fixVertical (mean + descender * 2)