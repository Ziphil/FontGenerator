--


module Data.FontGen.FontType
  ( Weight (..)
  , Slope (..)
  , Stretch (..)
  , Style (..)
  , FontInfo (..)
  , fullName
  , dirName
  )
where

import Data.Char
import Data.FontGen.GlyphType
import Data.List
import Data.Version


data Weight = Thin | ExtraLight | Light | Regular | Medium | SemiBold | Bold | ExtraBold | Heavy
  deriving (Eq, Show, Enum)

showWeight :: Weight -> String
showWeight = show

data Slope = Upright | Oblique | Italic
  deriving (Eq, Show, Enum)

showSlope :: Slope -> String
showSlope Upright = ""
showSlope slope = show slope

data Stretch = Compressed | Condensed | Normal | Extended
  deriving (Eq, Show, Enum)

showStretch :: Stretch -> String
showStretch Normal = ""
showStretch stretch = show stretch

data Style = Style {weight :: Weight, slope :: Slope, stretch :: Stretch}
  deriving (Eq, Show)

data FontInfo = FontInfo {family :: String, style :: Style, version :: Version, metrics :: Metrics, glyphs :: Glyphs}

fullName :: FontInfo -> String
fullName info = intercalate " " $ filter (not . null) $ [family info, weightString, slopeString, stretchString]
  where
    weightString = showWeight $ weight $ style info
    slopeString = showSlope $ slope $ style info
    stretchString = showStretch $ stretch $ style info

dirName :: FontInfo -> String
dirName info = map toLower $ intercalate "-" $ filter (not . null) $ [family info, weightString, slopeString, stretchString]
  where
    weightString = showWeight $ weight $ style info
    slopeString = showSlope $ slope $ style info
    stretchString = showStretch $ stretch $ style info