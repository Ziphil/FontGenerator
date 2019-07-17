{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Value
  ( thickness
  , gap
  , triangleWidth
  , triangleHeight
  , ascenderHeight
  , transphoneGap
  , diamondGap
  , spaceWidth
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config


thickness :: Given Config => Double
thickness = 80

-- 文字と文字の間隔を表します。
-- ただし、隣り合う三角形の斜辺部に垂直な方向の距離であり、水平距離ではありません。
gap :: Given Config => Double
gap = thickness * 1.1

triangleWidth :: Given Config => Double
triangleWidth = 500

triangleHeight :: Given Config => Double
triangleHeight = 500

-- ディセンダー部分の高さを表します。
-- フォントのデザインの統一感のため、この値はアセンダー部分の高さとしても利用されます。
ascenderHeight :: Given Config => Double
ascenderHeight = 250

transphoneGap :: Given Config => Double
transphoneGap = thickness * 1.1

diamondGap :: Given Config => Double
diamondGap = thickness * 1

spaceWidth :: Given Config => Double
spaceWidth = triangleWidth * 0.5