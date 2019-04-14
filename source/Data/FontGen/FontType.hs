--


module Data.FontGen.FontType
  ( Weight (..)
  , Slope (..)
  , Stretch (..)
  , Style (..)
  , FontInfo (..)
  )
where

import Data.FontGen.GlyphType
import Data.Version


data Weight = Thin | ExtraLight | Light | Regular | Medium | SemiBold | Bold | ExtraBold | Heavy
  deriving (Eq, Show, Enum)

data Slope = Upright | Oblique | Italic
  deriving (Eq, Show, Enum)

data Stretch = Compressed | Condensed | Normal | Extended
  deriving (Eq, Show, Enum)

data Style = Style {weight :: Weight, slope :: Slope, stretch :: Stretch}
  deriving (Eq, Show)

data FontInfo = FontInfo {family :: String, style :: Style, version :: Version, metrics :: Metrics, glyphs :: Glyphs}