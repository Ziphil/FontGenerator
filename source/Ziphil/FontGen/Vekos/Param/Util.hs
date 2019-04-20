{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Param.Util
  ( metrics
  , spacing
  , badekSpacing
  , makeGlyph'
  , makeGlyphWithSpacing'
  )
where

import Data.FontGen
import Data.Reflection
import Ziphil.FontGen.Vekos.Param.Config
import Ziphil.FontGen.Vekos.Param.Value


metrics :: Given Config => Metrics
metrics = with &~ do
  metricEm .= actualEm
  metricAscent .= actualAscent
  metricDescent .= actualDescent

spacing :: Given Config => Spacing
spacing = with &~ do
  leftBearing .= bearing
  rightBearing .= bearing

badekSpacing :: Given Config => Spacing
badekSpacing = with &~ do
  leftBearing .= badekBearing
  rightBearing .= badekBearing

-- パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスとスペーシングの情報を自動的に使用します。
makeGlyph' :: Given Config => [Part] -> Glyph
makeGlyph' = makeGlyph metrics spacing

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => Spacing -> [Part] -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics