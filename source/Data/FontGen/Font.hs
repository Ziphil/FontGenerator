{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FontGen.Font
  ( Weight (..)
  , Slope (..)
  , Stretch (..)
  , showWeight
  , showSlope
  , showStretch
  , Style, weight, slope, stretch
  , Font, family, copyright, version, style, metrics, glyphs
  , extendedFamily
  , postScriptName
  , fullName
  , dirName
  , Fonts
  , fontsBy
  )
where

import Control.Lens
import Data.Char
import Data.Default.Class
import Data.FontGen.Glyph
import Data.FontGen.Metrics
import Data.FontGen.Util.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
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

data Font = Font {_family :: String, _copyright :: String, _version :: Version, _style :: Style, _metrics :: Metrics, _glyphs :: Glyphs}

makeFieldsNoPrefix ''Font

instance Default Version where
  def = makeVersion [0, 0, 0]

instance Default Font where
  def = Font "Undefined" "None" def def def Map.empty

modifiers :: Font -> [String]
modifiers font = filter (not . null) [familyString, stretchString, weightString, slopeString]
  where
    familyString = font ^. family
    weightString = showWeight $ font ^. style . weight
    slopeString = showSlope $ font ^. style . slope
    stretchString = showStretch $ font ^. style . stretch

stretchModifiers :: Font -> [String]
stretchModifiers font = filter (not . null) [familyString, stretchString]
  where
    familyString = font ^. family
    stretchString = showStretch $ font ^. style . stretch

extendedFamily :: Getter Font String
extendedFamily = to $ intercalate " " . stretchModifiers

postScriptName :: Getter Font String
postScriptName = to $ filter (/= ' ') . intercalate "" . modifiers

fullName :: Getter Font String
fullName = to $ intercalate " " . modifiers

dirName :: Getter Font String
dirName = to $ map toLower . replace' " " "_" . intercalate "_" . modifiers
  where
    replace' before after = Text.unpack . Text.replace before after . Text.pack

type Fonts = Map String Font

fontsBy :: State Fonts () -> Fonts
fontsBy = flip execState Map.empty