{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Util
  ( metrics
  , spacing
  , makeGlyph'
  , makeGlyphWithSpacing'
  )
where

import Data.FontGen
import Data.Reflection
import Ziphil.FontGen.Vekos.Config
import Ziphil.FontGen.Vekos.Value


metrics :: Given Config => Metrics
metrics = with &~ do
  metricEm .= actualEm
  metricAscent .= actualAscent
  metricDescent .= actualDescent

spacing :: Given Config => Spacing
spacing = with &~ do
  leftBearing .= bearing
  rightBearing .= bearing

-- パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスとスペーシングの情報を自動的に使用します。
makeGlyph' :: Given Config => [Part] -> Glyph
makeGlyph' = makeGlyph metrics spacing

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => Spacing -> [Part] -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics