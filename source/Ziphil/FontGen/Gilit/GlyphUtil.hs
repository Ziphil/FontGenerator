{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.GlyphUtil
  ( metrics
  , spacingBy
  , singleSpacing
  , singleTransphoneSpacing
  , doubleSpacing
  , doubleTransphoneSpacing
  , makeGlyphWithSpacing'
  , reflectTriangle
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

spacingBy :: Given Config => Double -> WidthSpacing
spacingBy width = with &~ do
  leftX .= defaultLeftX
  fixedWidth .= width - widthDifference

singleSpacing :: Given Config => WidthSpacing
singleSpacing = spacingBy $ triangleWidth

singleTransphoneSpacing :: Given Config => WidthSpacing
singleTransphoneSpacing = spacingBy $ triangleWidth + horizontalTransphoneGap + thickness / sinA obliqueAngle

doubleSpacing :: Given Config => WidthSpacing
doubleSpacing = spacingBy $ triangleWidth * 3 / 2

doubleTransphoneSpacing :: Given Config => WidthSpacing
doubleTransphoneSpacing = spacingBy $ triangleWidth * 3 / 2 + horizontalTransphoneGap + thickness / sinA obliqueAngle

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => WidthSpacing -> [Part] -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics

-- 上に尖った三角形状の文字のパーツを反転させ、下に尖った三角形状の文字のパーツに変換します。
reflectTriangle :: Given Config => Part -> Part
reflectTriangle = reflectY >>> moveOriginBy (0 &| -triangleHeight)