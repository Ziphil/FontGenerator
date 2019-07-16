{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Util
  ( metrics
  , singleSpacing
  , makeGlyphWithSpacing'
  , makeGlyphs'
  , choose
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
makeGlyphWithSpacing' :: Given Config => WidthSpacing -> [Bool -> Part] -> Bool -> Glyph
makeGlyphWithSpacing' spacing parts flag = makeGlyph metrics spacing $ map ($ flag) parts

-- 対応する大文字がある場合に、その大文字には上下反転したグリフを割り当てるようにして、グリフマップを生成します。
-- 記号などの対応する大文字がないものに関しては、上下反転したグリフの方が登録されます。
makeGlyphs' :: [(Char, Bool -> Glyph)] -> Glyphs
makeGlyphs' list = makeGlyphs $ map (id *** ($ True)) list ++ map (toUpper *** ($ False)) list

choose :: Given Config => Part -> Bool -> Part
choose = flip $ bool id (reflectY . moveOriginBy (0 &| mean))