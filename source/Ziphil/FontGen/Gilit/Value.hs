{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Value
  ( gap
  , thickness
  , triangleWidth
  , triangleHeight
  , ascenderHeight
  , spaceWidth
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config


gap :: Given Config => Double
gap = 80

thickness :: Given Config => Double
thickness = 70

triangleWidth :: Given Config => Double
triangleWidth = 500

triangleHeight :: Given Config => Double
triangleHeight = 500

-- ディセンダー部分の高さを表します。
-- フォントのデザインの統一感のため、この値はアセンダー部分の高さとしても利用されます。
ascenderHeight :: Given Config => Double
ascenderHeight = 250

spaceWidth :: Given Config => Double
spaceWidth = triangleWidth * 0.5