--


module Ziphil.FontGen.Vekos.Param
  ( descent
  , mean
  , overshoot
  , bearing
  , weightX
  , weightY
  , bowlWidth
  , transphoneGap
  , spaceWidth
  )
where

import Data.FontGen


-- ディセンダーラインの深さを表します。
-- このフォントではディセンダー部分とアセンダー部分の高さが同じなので、アセンダーラインの高さとしても利用されます。
descent :: Double
descent = 250

-- ミーンラインの高さ (エックスハイト) を表します。
mean :: Double
mean = 500

overshoot :: Double
overshoot = 10

bearing :: Double
bearing = 30

weightX :: Double
weightX = 100

weightY :: Double
weightY = 75

bowlWidth :: Double
bowlWidth = 450

transphoneGap :: Double
transphoneGap = -10

spaceWidth :: Double
spaceWidth = 300