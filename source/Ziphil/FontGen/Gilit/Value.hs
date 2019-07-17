{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Value
  ( extraAscent
  , extraDescent
  , thickness
  , maxThickness
  , gap
  , triangleHeight
  , triangleWidth
  , ascenderHeight
  , transphoneGap
  , diamondGap
  , wedgeGap
  , spaceWidth
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config


extraAscent :: Given Config => Double
extraAscent = 0

extraDescent :: Given Config => Double
extraDescent = 0

thickness :: Given Config => Double
thickness = weightConst' * 80
  where
    weightConst' = given ^. weightConst

-- 同じファミリーにおけるストローク幅の最大値を表します。
-- このフォントは、グリフの設計上、ストローク幅によってアセンダーやディセンダーの高さが異なります。
-- したがって、フォントサイズを同じにすると、グリフ全体が拡大縮小され、ウェイトによって三角形部分の大きさが変わる可能性があります。
-- これを防ぐため、フォントファイルに登録されるアセントやディセントはこの値を用いて計算され、ウェイトを通して固定されます。
maxThickness :: Given Config => Double
maxThickness = 140

-- 文字と文字の間隔を表します。
-- ただし、隣り合う三角形の斜辺部に垂直な方向の距離であり、水平距離ではありません。
gap :: Given Config => Double
gap = thickness * 1.1

triangleHeight :: Given Config => Double
triangleHeight = 500

triangleWidth :: Given Config => Double
triangleWidth = triangleHeight * stretchRatio'
  where
    stretchRatio' = given ^. stretchRatio

-- ディセンダー部分の高さを表します。
-- フォントのデザインの統一感のため、この値はアセンダー部分の高さとしても利用されます。
ascenderHeight :: Given Config => Double
ascenderHeight = triangleHeight * ascenderRatio'
  where
    ascenderRatio' = given ^. ascenderRatio

transphoneGap :: Given Config => Double
transphoneGap = thickness * 1.1

diamondGap :: Given Config => Double
diamondGap = thickness * 1

wedgeGap :: Given Config => Double
wedgeGap = thickness * 1

spaceWidth :: Given Config => Double
spaceWidth = triangleWidth * 0.5