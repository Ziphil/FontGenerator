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
      [ partLes # translate ~: r2 (0, mean / 2 + descender)
      ]

glyphRes :: Glyph
glyphRes = makeGlyph bearing bearing $ mconcat diagrams
  where
    diagrams =
      [ partLes # rotate ~: (180 @@ deg) # translate ~: r2 (bowlWidth, mean / 2 + descender) 
      ]

makeGlyph :: Double -> Double -> Part -> Glyph
makeGlyph left right = addBearing left right . fixVertical (mean + descender * 2) . strokePath