{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Value
  ( extraAscent
  , extraDescent
  , thickness
  , maxThickness
  , maxObliqueAngle
  , gap
  , triangleHeight
  , triangleWidth
  , ascenderHeight
  , diamondGap
  , transphoneGap
  , acuteRatio
  , diacriticGap
  , diacriticOvershootRatio
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
-- このフォントは、グリフの設計上、ストロークの幅や三角形部分の幅によってアセンダーやディセンダーの高さが異なります。
-- したがって、フォントサイズを同じにすると、グリフ全体が拡大縮小され、ウェイトによって三角形部分の大きさが変わる可能性があります。
-- これを防ぐため、フォントファイルに登録されるアセントやディセントはこの値を用いて計算され、ウェイトを通して固定されます。
maxThickness :: Given Config => Double
maxThickness = 140

-- 同じファミリーにおける三角形の斜辺の角度の最大値を表します。
-- 上記と同じ理由のため、フォントファイルに登録されるアセントやディセントはこの値を用いて計算されます。
maxObliqueAngle :: Given Config => Angle Double
maxObliqueAngle = atanA 2

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

diamondGap :: Given Config => Double
diamondGap = thickness * 1

transphoneGap :: Given Config => Double
transphoneGap = thickness * 1.1

acuteRatio :: Given Config => Double
acuteRatio = 1.3

diacriticGap :: Given Config => Double
diacriticGap = triangleHeight * 0.15

diacriticOvershootRatio :: Given Config => Double
diacriticOvershootRatio = 0.3

spaceWidth :: Given Config => Double
spaceWidth = triangleWidth * 0.5