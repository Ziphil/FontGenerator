{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Kaleg.GlyphUtil
  ( metrics
  , spacing
  , makeGlyph'
  , makeGlyphWithSpacing'
  , makeGlyphs'
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
makeGlyph' :: Given Config => Part -> Glyph
makeGlyph' = makeGlyph metrics spacing

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => FixedSpacing -> Part -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics

-- 対応する大文字がある場合に、その大文字にも同じグリフを割り当てるようにして、グリフマップを生成します。
makeGlyphs' :: State Glyphs () -> Glyphs
makeGlyphs' = uncurry Map.union . (Map.mapKeys toUpper &&& id) . makeGlyphs