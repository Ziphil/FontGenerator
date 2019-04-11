--


module Ziphil.FontGen.Vekos.Param
  ( descent
  , mean
  , overshoot
  , bearing
  , weightX
  , weightY
  , acuteWeightX
  , acuteWeightY
  , circumflexWeightX
  , circumflexWeightY
  , bowlWidth
  , transphoneGap
  , acuteWidth
  , acuteHeight
  , circumflexWidth
  , circumflexHeight
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

acuteWeightX :: Double
acuteWeightX = 75

acuteWeightY :: Double
acuteWeightY = 60

circumflexWeightX :: Double
circumflexWeightX = 70

circumflexWeightY :: Double
circumflexWeightY = 55

bowlWidth :: Double
bowlWidth = 450

transphoneGap :: Double
transphoneGap = -10

acuteWidth :: Double
acuteWidth = 250

acuteHeight :: Double
acuteHeight = 100

circumflexWidth :: Double
circumflexWidth = 200

circumflexHeight :: Double
circumflexHeight = 180

diacriticGap :: Double
diacriticGap = 50

spaceWidth :: Double
spaceWidth = 300