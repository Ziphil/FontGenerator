--


module Data.FontGen.FontType
  ( FontMetrics (..)
  , FontInfo (..)
  )
where

import Data.FontGen.GlyphType
import Data.Map (Map)
import qualified Data.Map as Map


data FontMetrics = FontMetrics {em :: Double, ascent :: Double, descent :: Double}
  deriving (Eq, Show)

data FontInfo = FontInfo {family :: String, weight :: String, version :: String, metrics :: FontMetrics, glyphs :: Glyphs}