{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Util
  ( metrics
  , singleSpacing
  , makeGlyphWithSpacing'
  )
where

import Control.Arrow
import Data.Bool
import Data.Char
import Data.FontGen
import Data.Reflection
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.Value


metrics :: Given Config => Metrics
metrics = with &~ do
  metricEm .= actualEm
  metricAscent .= actualAscent
  metricDescent .= actualDescent

singleSpacing :: Given Config => WidthSpacing
singleSpacing = with &~ do
  leftX .= triangleWidth / 4 - gap / 2
  fixedWidth .= triangleWidth / 2 + gap

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => WidthSpacing -> [Part] -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics