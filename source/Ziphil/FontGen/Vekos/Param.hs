{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}


module Ziphil.FontGen.Vekos.Param
  ( Config (..)
  , regularConfig
  , descent
  , mean
  , em
  , ascent
  , overshoot
  , bearing
  , thicknessX
  , thicknessY
  , acuteThicknessX
  , acuteThicknessY
  , circumflexThicknessX
  , circumflexThicknessY
  , bowlWidth
  , transphoneGap
  , acuteWidth
  , acuteHeight
  , circumflexWidth
  , circumflexHeight
  , diacriticGap
  , dotWidth
  , dotGap
  , spaceWidth
  , metrics
  , spacing
  , makeGlyph'
  , makeGlyphWithSpacing'
  )
where

import Data.FontGen.GlyphType
import Data.Reflection


data Config = Config {weightConst :: Double}
  deriving (Eq, Show)

regularConfig :: Config
regularConfig = Config {weightConst = 1}

-- ディセンダーラインの深さを表します。
-- このフォントではディセンダー部分とアセンダー部分の高さが同じなので、アセンダーラインの高さとしても利用されます。
descent :: Given Config => Double
descent = 250

-- ミーンラインの高さ (エックスハイト) を表します。
mean :: Given Config => Double
mean = 500

em :: Given Config => Double
em = mean + descent * 2

ascent :: Given Config => Double
ascent = mean + descent

overshoot :: Given Config => Double
overshoot = 10

bearing :: Given Config => Double
bearing = 30

thicknessX :: Given Config => Double
thicknessX = 100

thicknessY :: Given Config => Double
thicknessY = 75

acuteThicknessX :: Given Config => Double
acuteThicknessX = 75

acuteThicknessY :: Given Config => Double
acuteThicknessY = 60

circumflexThicknessX :: Given Config => Double
circumflexThicknessX = 70

circumflexThicknessY :: Given Config => Double
circumflexThicknessY = 55

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

dotWidth :: Given Config => Double
dotWidth = thicknessX * 1.3

dotGap :: Given Config => Double
dotGap = dotWidth * 0.3

spaceWidth :: Given Config => Double
spaceWidth = 300

metrics :: Given Config => Metrics
metrics = Metrics {metricEm = em, metricAscent = ascent, metricDescent = descent}

spacing :: Given Config => Spacing
spacing = Spacing {leftBearing = bearing, rightBearing = bearing}

-- パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスとスペーシングの情報を自動的に使用します。
makeGlyph' :: Given Config => [Part] -> Glyph
makeGlyph' = makeGlyph metrics spacing

-- 与えられたスペーシングの情報を用いて、パーツのリストからグリフを生成します。
-- このとき、デフォルトのメトリクスの情報を自動的に使用します。
makeGlyphWithSpacing' :: Given Config => Spacing -> [Part] -> Glyph
makeGlyphWithSpacing' = makeGlyph metrics