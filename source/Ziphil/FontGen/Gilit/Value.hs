{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Value
  ( thickness
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


thickness :: Given Config => Double
thickness = weightConst' * 80
  where
    weightConst' = given ^. weightConst

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