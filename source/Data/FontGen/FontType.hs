--


module Data.FontGen.FontType
  ( FontMetrics (..)
  , Weight (..)
  , FontInfo (..)
  )
where

import Data.FontGen.GlyphType
import Data.Version


data FontMetrics = FontMetrics {metricEm :: Double, metricAscent :: Double, metricDescent :: Double}
  deriving (Eq, Show)

data Weight = Thin | ExtraLight | Light | Regular | Medium | SemiBold | Bold | ExtraBold | Heavy
  deriving (Eq, Show, Enum)

data FontInfo = FontInfo {family :: String, weight :: Weight, version :: Version, metrics :: FontMetrics, glyphs :: Glyphs}