{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.GlyphUtil
  ( metrics
  , defaultSpacing
  , transphoneSpacing
  , singleSpacing
  , singleTransphoneSpacing
  , doubleSpacing
  , doubleTransphoneSpacing
  , makeGlyphWithSpacing'
  )
where

import Control.Arrow
import Data.Bool
import Data.Char
import Data.FontGen
import Data.Reflection
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.Part
import Ziphil.FontGen.Gilit.Value


metrics :: Given Config => Metrics
metrics = with &~ do
  metricEm .= actualEm
  metricAscent .= actualAscent
  metricDescent .= actualDescent

defaultSpacing :: Given Config => WidthSpacing
defaultSpacing = with &~ do
  leftEnd .= defaultLeftEnd
  fixedWidth .= -widthDifference

transphoneSpacing :: Given Config => WidthSpacing
transphoneSpacing = with &~ do
  fixedWidth += horizontalTransphoneGap + thickness / sinA obliqueAngle

singleSpacing :: Given Config => WidthSpacing
singleSpacing = defaultSpacing &~ do
  fixedWidth += triangleWidth

singleTransphoneSpacing :: Given Config => WidthSpacing
singleTransphoneSpacing = singleSpacing ^+^ transphoneSpacing

doubleSpacing :: Given Config => WidthSpacing
doubleSpacing = defaultSpacing &~ do
  fixedWidth += triangleWidth * 3 / 2

doubleTransphoneSpacing :: Given Config => WidthSpacing
doubleTransphoneSpacing = doubleSpacing ^+^ transphoneSpacing

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => WidthSpacing -> Part -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics