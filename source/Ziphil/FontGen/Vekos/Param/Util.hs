{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Param.Util
  ( metrics
  , spacing
  , makeGlyph'
  , makeGlyphWithSpacing'
  )
where

import Data.FontGen.GlyphType
import Data.Reflection
import Ziphil.FontGen.Vekos.Param.Config
import Ziphil.FontGen.Vekos.Param.Value


metrics :: Given Config => Metrics
metrics = Metrics {metricEm = em, metricAscent = ascent, metricDescent = descent}

spacing :: Given Config => Spacing
spacing = Spacing {leftBearing = bearing, rightBearing = bearing}

-- パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスとスペーシングの情報を自動的に使用します。
makeGlyph' :: Given Config => [Part] -> Glyph
makeGlyph' = makeGlyph metrics spacing

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => Spacing -> [Part] -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics