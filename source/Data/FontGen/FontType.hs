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
  , FontInfo, family, copyright, version, style, metrics, glyphs
  , extendedFamily
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

data FontInfo = FontInfo {_family :: String, _copyright :: String, _version :: Version, _style :: Style, _metrics :: Metrics, _glyphs :: Glyphs}

makeFieldsNoPrefix ''FontInfo

instance Default Version where
  def = makeVersion [0, 0, 0]

instance Default FontInfo where
  def = FontInfo "Undefined" "None" def def def Map.empty

modifiers :: FontInfo -> [String]
modifiers info = filter (not . null) [familyString, stretchString, weightString, slopeString]
  where
    familyString = info ^. family
    weightString = showWeight $ info ^. style . weight
    slopeString = showSlope $ info ^. style . slope
    stretchString = showStretch $ info ^. style . stretch

stretchModifiers :: FontInfo -> [String]
stretchModifiers info = filter (not . null) [familyString, stretchString]
  where
    familyString = info ^. family
    stretchString = showStretch $ info ^. style . stretch

extendedFamily :: Getter FontInfo String
extendedFamily = to extendedFamily'
  where
    extendedFamily' info = intercalate " " $ stretchModifiers info

fullName :: Getter FontInfo String
fullName = to fullName'
  where
    fullName' info = intercalate " " $ modifiers info

dirName :: Getter FontInfo String
dirName = to dirName'
  where
    dirName' info = map toLower $ intercalate "-" $ modifiers info