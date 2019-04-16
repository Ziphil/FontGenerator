{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Param.Value
  ( descent
  , mean
  , em
  , ascent
  , overshoot
  , bearing
  , thicknessX
  , thicknessY
  , bowlWidth
  , tailBend
  , legBend
  , beakWidth
  , beakHeight
  , narrowBowlVirtualWidth
  , narrowBowlCorrection
  , nesLegBend
  , spineWidth
  , itTailBend
  , linkWidth
  , linkUpperCorrection
  , linkLowerCorrection
  , utTailBend
  , transphoneThicknessX
  , transphoneBend
  , transphoneGap
  , acuteThicknessX
  , acuteThicknessY
  , acuteWidth
  , acuteHeight
  , circumflexThicknessX
  , circumflexThicknessY
  , circumflexWidth
  , circumflexHeight
  , diacriticGap
  , dotWidth
  , dotGap
  , spaceWidth
  )
where

import Data.FontGen.GlyphType
import Data.Reflection
import Ziphil.FontGen.Vekos.Param.Config


-- ディセンダーラインの深さを表します。
-- このフォントではディセンダー部分とアセンダー部分の高さが同じなので、アセンダーラインとミーンラインの距離としても利用されます。
descent :: Given Config => Double
descent = 250

-- ミーンラインの高さを表します。
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

bowlWidth :: Given Config => Double
bowlWidth = 450

-- l の文字のディセンダー部分が左に曲がる距離を表します。
tailBend :: Given Config => Double
tailBend = bowlWidth * 0.5

-- y の文字の下半分が内側に曲がる距離を表します。
legBend :: Given Config => Double
legBend = bowlWidth * 0.15

beakWidth :: Given Config => Double
beakWidth = bowlWidth / 2 * 0.95
  
beakHeight :: Given Config => Double
beakHeight = mean * 0.35

-- x, j の文字に共通する細い丸い部分の見た目の幅を表します。
-- 実際に作られるパーツの幅は、2 つ重ねたときに重なった部分が太く見えないよう補正されるので、この値より小さくなります。
narrowBowlVirtualWidth :: Given Config => Double
narrowBowlVirtualWidth = bowlWidth * 0.9

narrowBowlCorrection :: Given Config => Double
narrowBowlCorrection = thicknessX * 0.15

-- n の文字の書き終わりと書き始めの箇所が内側に曲がる距離を表します。
nesLegBend :: Given Config => Double
nesLegBend = legBend

spineWidth :: Given Config => Double
spineWidth = bowlWidth * 0.5

-- i の文字のディセンダー部分が右に曲がる距離を表します。
itTailBend :: Given Config => Double
itTailBend = bowlWidth * 0.5

linkWidth :: Given Config => Double
linkWidth = bowlWidth * 0.8

linkUpperCorrection :: Given Config => Double
linkUpperCorrection = thicknessY * 0.1

linkLowerCorrection :: Given Config => Double
linkLowerCorrection = thicknessY * 0.1

-- u の文字のディセンダー部分が左に曲がる距離を表します。
utTailBend :: Given Config => Double
utTailBend = bowlWidth * 0.55

transphoneThicknessX :: Given Config => Double
transphoneThicknessX = thicknessX * 0.95

transphoneBend :: Given Config => Double
transphoneBend = bowlWidth * 0.15

transphoneGap :: Given Config => Double
transphoneGap = -10

acuteThicknessX :: Given Config => Double
acuteThicknessX = 75

acuteThicknessY :: Given Config => Double
acuteThicknessY = 60

acuteWidth :: Given Config => Double
acuteWidth = 250

acuteHeight :: Given Config => Double
acuteHeight = 100

circumflexThicknessX :: Given Config => Double
circumflexThicknessX = 70

circumflexThicknessY :: Given Config => Double
circumflexThicknessY = 55

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