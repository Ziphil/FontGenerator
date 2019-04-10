--


module Ziphil.FontGen.Vekos.Param
  ( descent
  , mean
  , overshoot
  , bearing
  , weightX
  , weightY
  , diacriticWeightX
  , diacriticWeightY
  , bowlWidth
  , transphoneGap
  , diacriticWidth
  , diacriticHeight
  , diacriticGap
  , spaceWidth
  )
where


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

diacriticWeightX :: Double
diacriticWeightX = 75

diacriticWeightY :: Double
diacriticWeightY = 60

bowlWidth :: Double
bowlWidth = 450

transphoneGap :: Double
transphoneGap = -10

diacriticWidth :: Double
diacriticWidth = 250

diacriticHeight :: Double
diacriticHeight = 100

diacriticGap :: Double
diacriticGap = 100

spaceWidth :: Double
spaceWidth = 300