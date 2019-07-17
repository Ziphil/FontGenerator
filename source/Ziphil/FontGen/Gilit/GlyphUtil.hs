{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.GlyphUtil
  ( metrics
  , singleSpacing
  , singleTransphoneSpacing
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

singleSpacing :: Given Config => WidthSpacing
singleSpacing = with &~ do
  leftX .= defaultLeftX
  fixedWidth .= triangleWidth - widthDifference

singleTransphoneSpacing :: Given Config => WidthSpacing
singleTransphoneSpacing = with &~ do
  leftX .= defaultLeftX
  fixedWidth .= triangleWidth + horizontalTransphoneGap + thickness / (sinA obliqueAngle) - widthDifference

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => WidthSpacing -> [Part] -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics

-- 上に尖った三角形状の文字のパーツを反転させ、下に尖った三角形状の文字のパーツに変換します。
reflectTriangle :: Given Config => Part -> Part
reflectTriangle = reflectY >>> moveOriginBy (0 &| -triangleHeight)