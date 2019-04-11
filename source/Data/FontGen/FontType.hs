--


module Data.FontGen.FontType
  ( FontInfo (..)
  )
where

import Data.FontGen.GlyphType
import Data.Version


data FontInfo = FontInfo {family :: String, weight :: Weight, version :: Version, metrics :: Metrics, glyphs :: Glyphs}