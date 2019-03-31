--


module Ziphil.Font.Vekos.Param
  ( descender
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

import Ziphil.Util.Core


-- ディセンダーラインの深さを表します。
-- このフォントではディセンダー部分とアセンダー部分の高さが同じなので、アセンダーラインの高さとしても利用されます。
descender :: Double
descender = 250

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