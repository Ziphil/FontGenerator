{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.FontGen.FontType
  ( Weight (..)
  , Slope (..)
  , Stretch (..)
  , showWeight
  , showSlope
  , showStretch
  , Style, weight, slope, stretch
  , FontInfo, family, style, version, metrics, glyphs
  , fullName
  , dirName
  )
where

import Control.Lens
import Data.Char
import Data.Default.Class
import Data.FontGen.GlyphType
import Data.List
import qualified Data.Map as Map
import Data.Version


data Weight = Thin | ExtraLight | Light | Regular | Medium | SemiBold | Bold | ExtraBold | Heavy
  deriving (Eq, Show, Enum)

data Slope = Upright | Oblique | Italic
  deriving (Eq, Show, Enum)

data Stretch = Compressed | Condensed | Normal | Extended
  deriving (Eq, Show, Enum)

showWeight :: Weight -> String
showWeight = show

showSlope :: Slope -> String
showSlope Upright = ""
showSlope slope = show slope

showStretch :: Stretch -> String
showStretch Normal = ""
showStretch stretch = show stretch

data Style = Style {_weight :: Weight, _slope :: Slope, _stretch :: Stretch}
  deriving (Eq, Show)

makeFieldsNoPrefix ''Style

instance Default Style where
  def = Style Regular Upright Normal

data FontInfo = FontInfo {_family :: String, _style :: Style, _version :: Version, _metrics :: Metrics, _glyphs :: Glyphs}

makeFieldsNoPrefix ''FontInfo

instance Default Version where
  def = makeVersion [0, 0, 0]

instance Default FontInfo where
  def = FontInfo "Undefined" def def def Map.empty

modifiers :: FontInfo -> [String]
modifiers info = filter (not . null) [familyString, weightString, slopeString, stretchString]
  where
    familyString = info ^. family
    weightString = showWeight $ info ^. style . weight
    slopeString = showSlope $ info ^. style . slope
    stretchString = showStretch $ info ^. style . stretch

fullName :: FontInfo -> String
fullName info = intercalate " " $ modifiers info

dirName :: FontInfo -> String
dirName info = map toLower $ intercalate "-" $ modifiers info