--


module Data.FontGen.FontType
  ( Weight (..)
  , Style (..)
  , Stretch (..)
  , FontInfo (..)
  )
where

import Data.FontGen.GlyphType
import Data.Version


data Weight = Thin | ExtraLight | Light | Regular | Medium | SemiBold | Bold | ExtraBold | Heavy
  deriving (Eq, Show, Enum)

data Style = Upright | Oblique | Italic
  deriving (Eq, Show, Enum)

data Stretch = Compressed | Condensed | Normal | Extended
  deriving (Eq, Show, Enum)

data FontInfo = FontInfo {family :: String, weight :: Weight, version :: Version, metrics :: Metrics, glyphs :: Glyphs}