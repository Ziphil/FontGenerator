{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Vekos.Value
  ( descent
  , mean
  , extraDescent
  , extraAscent
  , overshoot
  , bearing
  , thicknessX
  , thicknessY
  , bowlWidth
  , lesTailBend
  , lesTailCorrection
  , yesLegBend
  , talBeakWidth
  , talBeakHeight
  , narrowBowlVirtualWidth
  , narrowBowlCorrection
  , nesLegBend
  , spineWidth
  , itTailBend
  , linkWidth
  , linkUpperCorrection
  , linkLowerCorrection
  , utTailBend
  , solidusThicknessRatio
  , solidusGrade
  , xefBeakWidth
  , xefBeakHeight
  , tasBeakWidth
  , tasBeakHeight
  , tasShoulderWidth
  , tasShoulderStraightHeight
  , tasCrossbarAltitude
  , yusWidth
  , yusLegBend 
  , yusShoulderStraightWidth 
  , yusCrossbarThicknessRatio
  , yusCrossbarLatitude
  , transphoneThicknessRatio
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
  , badekLeftBearing
  , badekGap
  , padekBend
  , nokHeight
  , dikakRightBearing
  , dikakBend
  , dikakHeight
  , middotAltitude
  , kaltakBearing
  , upperKaltakAltitude
  , fekAltitude
  , fekWidth
  , fohakWidth
  , dashAltitude
  , dashWidth
  , rakutWidth
  , rakutHeight
  , spaceWidth
  )
where

import Data.FontGen
import Ziphil.FontGen.Vekos.Config


-- ディセンダーラインの深さを表します。
-- このフォントではディセンダー部分とアセンダー部分の高さが同じなので、アセンダーラインとミーンラインの距離としても利用されます。
descent :: Given Config => Double
descent = 250

-- ミーンラインの高さを表します。
mean :: Given Config => Double
mean = 500

extraDescent :: Given Config => Double
extraDescent = 40

extraAscent :: Given Config => Double
extraAscent = 10

overshoot :: Given Config => Double
overshoot = 10

bearing :: Given Config => Double
bearing = bowlWidth * 0.09

thicknessX :: Given Config => Double
thicknessX = weightConst' * 100
  where
    weightConst' = given ^. weightConst

thicknessY :: Given Config => Double
thicknessY = thicknessX * contrastRatio'
  where
    contrastRatio' = given ^. contrastRatio

bowlWidth :: Given Config => Double
bowlWidth = (weightConst' * 80 + 370) * stretchConst'
  where
    weightConst' = given ^. weightConst
    stretchConst' = given ^. stretchConst

-- l の文字のディセンダー部分について、その先端の中央と上にある丸い部分の右端との水平距離を表します。
lesTailBend :: Given Config => Double
lesTailBend = bowlWidth * 0.6

lesTailCorrection :: Given Config => Double
lesTailCorrection = thicknessX * 0.3

-- y の文字の下半分の内側に曲がる部分について、その最下部の外側の端と丸い部分の端との水平距離を表します。
-- 曲線の外側の曲がり具合を指定しているので、線の太さが大きくなるとより内側に曲がることに注意してください。
yesLegBend :: Given Config => Double
yesLegBend = bowlWidth * 0.15

talBeakWidth :: Given Config => Double
talBeakWidth = bowlWidth / 2 * 0.95
  
talBeakHeight :: Given Config => Double
talBeakHeight = mean * 0.35

-- x, j の文字に共通する細い丸い部分の見た目の幅を表します。
-- 実際に作られるパーツの幅は、2 つ重ねたときに重なった部分が太く見えないよう補正されるので、この値より小さくなります。
narrowBowlVirtualWidth :: Given Config => Double
narrowBowlVirtualWidth = bowlWidth * 0.9

narrowBowlCorrection :: Given Config => Double
narrowBowlCorrection = thicknessX * 0.15

-- n の文字の書き終わりと書き始めの箇所について、その先端の外側の端と丸い部分の端との水平距離を表します。
-- 曲線の外側の曲がり具合を指定しているので、線の太さが大きくなるとより内側に曲がることに注意してください。
nesLegBend :: Given Config => Double
nesLegBend = yesLegBend

spineWidth :: Given Config => Double
spineWidth = bowlWidth * 0.5

-- i の文字のディセンダー部分について、その先端の中央と上にある丸い部分の左端との水平距離を表します。
itTailBend :: Given Config => Double
itTailBend = bowlWidth * 0.6

linkWidth :: Given Config => Double
linkWidth = bowlWidth * 0.8

linkUpperCorrection :: Given Config => Double
linkUpperCorrection = thicknessY * 0.1

linkLowerCorrection :: Given Config => Double
linkLowerCorrection = thicknessY * 0.1

-- u の文字のディセンダー部分について、その先端の中央と上にある折れ曲がる部分の右端との水平距離を表します。
utTailBend :: Given Config => Double
utTailBend = bowlWidth * 0.45

-- 0 の文字の斜線の部分の太さに乗算する係数を表します。
solidusThicknessRatio :: Given Config => Double
solidusThicknessRatio = min 1 (-weightConst' * 0.12 + 1.084)
  where
    weightConst' = given ^. weightConst

solidusGrade :: Given Config => Double
solidusGrade = mean / 2 * 0.8

xefBeakWidth :: Given Config => Double
xefBeakWidth = narrowBowlVirtualWidth / 2 * 0.95
  
xefBeakHeight :: Given Config => Double
xefBeakHeight = mean * 0.35

tasBeakWidth :: Given Config => Double
tasBeakWidth = talBeakWidth

tasBeakHeight :: Given Config => Double
tasBeakHeight = mean * 0.3

tasShoulderWidth :: Given Config => Double
tasShoulderWidth = bowlWidth / 2 * 1

-- 1 の文字の右下にある中央の横線と繋がる部分に含まれる直線部分の長さを表します。
-- この部分のアウトラインを単純に 1 つの曲線としてしまうと尖って見えてしまうため、途中から垂直な直線に連結させています。
-- その垂直な直線部分の長さを指定します。
tasShoulderStraightHeight :: Given Config => Double
tasShoulderStraightHeight = thicknessY * 0.5

-- 1 の文字の横棒について、その鉛直方向中央とベースラインとの鉛直距離を表します。
tasCrossbarAltitude :: Given Config => Double
tasCrossbarAltitude = mean * 0.45

yusWidth :: Given Config => Double
yusWidth = bowlWidth * 1.3

yusLegBend :: Given Config => Double
yusLegBend = yesLegBend

yusShoulderStraightWidth :: Given Config => Double
yusShoulderStraightWidth = thicknessX * yusCrossbarThicknessRatio * 0.7

yusCrossbarThicknessRatio :: Given Config => Double
yusCrossbarThicknessRatio = 1

yusCrossbarLatitude :: Given Config => Double
yusCrossbarLatitude = yusWidth / 2 * 0.95

transphoneThicknessRatio :: Given Config => Double
transphoneThicknessRatio = 0.95

-- 変音符が左側もしくは右側に曲がる水平距離を表します。
transphoneBend :: Given Config => Double
transphoneBend = bowlWidth * 0.15

transphoneGap :: Given Config => Double
transphoneGap = bowlWidth * 0.18

acuteThicknessX :: Given Config => Double
acuteThicknessX = min (weightConst' * 90) (weightConst' * 40 + 35)
  where
    weightConst' = given ^. weightConst

acuteThicknessY :: Given Config => Double
acuteThicknessY = acuteThicknessX * contrastRatio'
  where
    contrastRatio' = given ^. contrastRatio

acuteWidth :: Given Config => Double
acuteWidth = bowlWidth * 0.6

acuteHeight :: Given Config => Double
acuteHeight = descent * 0.55

circumflexThicknessX :: Given Config => Double
circumflexThicknessX = min (weightConst' * 90) (weightConst' * 40 + 35)
  where
    weightConst' = given ^. weightConst

circumflexThicknessY :: Given Config => Double
circumflexThicknessY = circumflexThicknessX * contrastRatio'
  where
    contrastRatio' = given ^. contrastRatio

circumflexWidth :: Given Config => Double
circumflexWidth = bowlWidth * 0.5

circumflexHeight :: Given Config => Double
circumflexHeight = descent * 0.75

diacriticGap :: Given Config => Double
diacriticGap = descent * 0.25

dotWidth :: Given Config => Double
dotWidth = min (weightConst' * 150) (weightConst' * 100 + 30)
  where
    weightConst' = given ^. weightConst

dotGap :: Given Config => Double
dotGap = bowlWidth * 0.09

badekLeftBearing :: Given Config => Double
badekLeftBearing = bearing * 1.8

badekGap :: Given Config => Double
badekGap = (mean + descent) * 0.13

padekBend :: Given Config => Double
padekBend = min (dotWidth + dotGap) (bowlWidth * 0.3)

nokHeight :: Given Config => Double
nokHeight = (mean + descent) * 0.3

dikakRightBearing :: Given Config => Double
dikakRightBearing = -bearing * 0.5

dikakBend :: Given Config => Double
dikakBend = bowlWidth * 0.15

dikakHeight :: Given Config => Double
dikakHeight = (mean + descent) * 0.3

middotAltitude :: Given Config => Double
middotAltitude = mean / 2

kaltakBearing :: Given Config => Double
kaltakBearing = bearing * 1.8

upperKaltakAltitude :: Given Config => Double
upperKaltakAltitude = mean * 0.7

-- フェークの鉛直方向中央とベースラインとの鉛直距離を表します。
fekAltitude :: Given Config => Double
fekAltitude = mean / 2

fekWidth :: Given Config => Double
fekWidth = bowlWidth * 0.6

fohakWidth :: Given Config => Double
fohakWidth = bowlWidth * 1.5

dashAltitude :: Given Config => Double
dashAltitude = mean / 2

dashWidth :: Given Config => Double
dashWidth = bowlWidth * 2

rakutWidth :: Given Config => Double
rakutWidth = bowlWidth * 0.55

rakutHeight :: Given Config => Double
rakutHeight = (mean + descent) * 0.6

spaceWidth :: Given Config => Double
spaceWidth = bowlWidth * 0.55