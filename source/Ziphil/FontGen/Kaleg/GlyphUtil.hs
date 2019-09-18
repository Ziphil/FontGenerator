{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Kaleg.GlyphUtil
  ( metrics
  , spacing
  , glyphBy'
  , glyphByWith'
  , glyphsBy'
  )
where

import Control.Arrow
import Data.Char
import Data.FontGen
import qualified Data.Map as Map
import Data.Reflection
import Ziphil.FontGen.Kaleg.Config
import Ziphil.FontGen.Kaleg.Part
import Ziphil.FontGen.Kaleg.Value


metrics :: Given Config => Metrics
metrics = with &~ do
  metricEm .= actualEm
  metricAscent .= actualAscent
  metricDescent .= actualDescent

spacing :: Given Config => FixedSpacing
spacing = with &~ do
  leftBearing .= bearing
  rightBearing .= bearing

-- パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスとスペーシングの情報を自動的に使用します。
glyphBy' :: Given Config => Part -> Glyph
glyphBy' = glyphBy metrics spacing

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
glyphByWith' :: Given Config => FixedSpacing -> Part -> Glyph
glyphByWith' = glyphBy metrics

-- 対応する大文字がある場合に、その大文字にも同じグリフを割り当てるようにして、グリフマップを生成します。
glyphsBy' :: State Glyphs () -> Glyphs
glyphsBy' = uncurry Map.union . (Map.mapKeys toUpper &&& id) . glyphsBy