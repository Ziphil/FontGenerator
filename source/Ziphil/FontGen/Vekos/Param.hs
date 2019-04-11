{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}


module Ziphil.FontGen.Vekos.Param
  ( Config (..)
  , regularConfig
  , descent
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

import Data.FontGen.FontType (Weight (..))
import Data.Reflection


data Config = Config {weight :: Weight}
  deriving (Eq, Show)

regularConfig :: Config
regularConfig = Config {weight}
  where
    weight = Regular

-- ディセンダーラインの深さを表します。
-- このフォントではディセンダー部分とアセンダー部分の高さが同じなので、アセンダーラインの高さとしても利用されます。
descent :: Given Config => Double
descent = 250

-- ミーンラインの高さ (エックスハイト) を表します。
mean :: Given Config => Double
mean = 500

overshoot :: Given Config => Double
overshoot = 10

bearing :: Given Config => Double
bearing = 30

weightX :: Given Config => Double
weightX = 100

weightY :: Given Config => Double
weightY = 75

acuteWeightX :: Given Config => Double
acuteWeightX = 75

acuteWeightY :: Given Config => Double
acuteWeightY = 60

circumflexWeightX :: Given Config => Double
circumflexWeightX = 70

circumflexWeightY :: Given Config => Double
circumflexWeightY = 55

bowlWidth :: Given Config => Double
bowlWidth = 450

transphoneGap :: Given Config => Double
transphoneGap = -10

acuteWidth :: Given Config => Double
acuteWidth = 250

acuteHeight :: Given Config => Double
acuteHeight = 100

circumflexWidth :: Given Config => Double
circumflexWidth = 200

circumflexHeight :: Given Config => Double
circumflexHeight = 180

diacriticGap :: Given Config => Double
diacriticGap = 50

spaceWidth :: Given Config => Double
spaceWidth = 300