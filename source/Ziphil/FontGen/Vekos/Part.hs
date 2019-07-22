{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Part
  ( partBowl
  , partLesTail
  , partLes
  , partYes
  , partTal
  , partNarrowBowl
  , partXal
  , partNes
  , partIt
  , partUpperUt
  , partUtTail
  , partUt
  , partRac
  , partSolidus
  , partNuf
  , partXefHalf
  , partXef
  , partTasFrame
  , partTasCrossbar
  , partTas
  , partYusFrame
  , partYusCrossbar
  , partYus
  , partTransphone
  , partAcute
  , partCircumflex
  , partDot
  , partFloatingDot
  , partBadekStem
  , partPadekStem
  , partNok
  , partDikak
  , partFek
  , partFohak
  , partDash
  , partOpeningRakut
  , talWidth
  , narrowBowlWidth
  , xalWidth
  , nesWidth
  , xefWidth
  , tasWidth
  , ascent
  , em
  , actualDescent
  , actualAscent
  , actualEm
  )
where

import Data.FontGen
import Ziphil.FontGen.Vekos.Config
import Ziphil.FontGen.Vekos.PartFunc
import Ziphil.FontGen.Vekos.Value


-- k, p, c, l, a などの文字に共通する丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
rimOuterBowl :: Given Config => Rim
rimOuterBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = bowlWidth / 2
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- k, p, c, l, a などの文字に共通する丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
rimInnerBowl :: Given Config => Rim
rimInnerBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = bowlWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot
    leftCont = height * 0.1
    topCont = width

-- k, p, c, l, a などの文字に共通する丸い部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partBowl :: Given Config => Part
partBowl = concatPart parts # moveOriginBy (originX &| 0)
  where
    outerRims = do
      rimOuterBowl # reflectY
      rimOuterBowl # rotateHalfTurn # backward
      rimOuterBowl # reflectX
      rimOuterBowl # backward
    innerRims = do
      rimInnerBowl # reflectY
      rimInnerBowl # rotateHalfTurn # backward
      rimInnerBowl # reflectX
      rimInnerBowl # backward
    parts = do
      makePart outerRims
      makePart innerRims # backward # translate (thicknessX &| 0)
    originX = bowlWidth / 2

-- l の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
rimLeftLesTail :: Given Config => Rim
rimLeftLesTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = lesTailBend - thicknessX / 2 + lesTailCorrection
    virtualBend = lesTailBend
    height = mean / 2 + descent
    topCont = searchTailInnerCont virtualBend height bottomCont
    bottomCont = descent * 1.08

-- l の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
rimRightLesTail :: Given Config => Rim
rimRightLesTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = lesTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = descent * 1.08
    bottomCont = searchTailInnerCont bend height topCont

-- 文字の書き始めや書き終わりの位置にある水平に切られた部分を、左端から右端への向きで生成します。
rimCut :: Given Config => Rim
rimCut = origin ~~ (width &| 0)
  where
    width = thicknessX

-- l の文字のディセンダーを生成します。
-- 反転や回転を施すことで、c などの文字のディセンダーや k, p などの文字のアセンダーとしても使えます。
-- 丸い部分と重ねたときに重なった部分が太く見えすぎないように、左側を少し細く補正してあります。
-- 原点は補正がないとしたときの左上の角にあります。
partLesTail :: Given Config => Part
partLesTail = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimLeftLesTail
      rimCut
      rimRightLesTail # backward
      makeRim $ origin ~~ (-thicknessX + lesTailCorrection &| 0)
    originX = -lesTailCorrection

-- l の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partLes :: Given Config => Part
partLes = parts
  where
    parts = do
      partBowl
      partLesTail # translate (bowlWidth / 2 - thicknessX &| 0)

-- y の文字の下半分にある曲線を、上端から下端への向きで生成します。
rimYesLeg :: Given Config => Rim
rimYesLeg = origin ~> (0 &| -leftCont) ~~ zero <~ (bend &| -height)
  where
    bend = yesLegBend
    height = mean / 2
    leftCont = height * 0.6

-- y の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partYes :: Given Config => Part
partYes = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimYesLeg
      rimCut
      rimYesLeg # backward
      rimInnerBowl
      rimInnerBowl # reflectX # backward
      rimYesLeg # reflectX
      rimCut
      rimYesLeg # reflectX # backward
      rimOuterBowl # reflectX
      rimOuterBowl # backward
    originX = bowlWidth / 2

talWidth :: Given Config => Double
talWidth = bowlWidth / 2 + talBeakWidth

-- t の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
rimOuterTalBeak :: Given Config => Rim
rimOuterTalBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = talBeakWidth
    height = talBeakHeight + overshoot
    rightCont = height * 0.05
    topCont = width

-- t の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
rimInnerTalBeak :: Given Config => Rim
rimInnerTalBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = talBeakWidth - thicknessX
    height = talBeakHeight - thicknessY + overshoot
    rightCont = height * 0.05
    topCont = width

-- t の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTal :: Given Config => Part
partTal = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimOuterBowl # reflectY
      rimOuterTalBeak # reflectY # backward
      rimCut # backward
      rimInnerTalBeak # reflectY
      rimInnerBowl # reflectY # backward
      rimInnerBowl
      rimInnerTalBeak # backward
      rimCut
      rimOuterTalBeak
      rimOuterBowl # backward
    originX = talWidth / 2

narrowBowlWidth :: Given Config => Double
narrowBowlWidth = narrowBowlVirtualWidth - narrowBowlCorrection

xalWidth :: Given Config => Double
xalWidth = narrowBowlVirtualWidth * 2 - thicknessX

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
rimOuterLeftNarrowBowl :: Given Config => Rim
rimOuterLeftNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、右端から上端への向きで生成します。
-- ただし、他のトレイルと使い方を揃えるため、左右反転してあります。
rimOuterRightNarrowBowl :: Given Config => Rim
rimOuterRightNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
rimInnerNarrowBowl :: Given Config => Rim
rimInnerNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分を生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は全体の中央にあります。
partNarrowBowl :: Given Config => Part
partNarrowBowl = concatPart parts # moveOriginBy (originX &| 0)
  where
    outerRims = do
      rimOuterLeftNarrowBowl # reflectY
      rimOuterRightNarrowBowl # rotateHalfTurn # backward
      rimOuterRightNarrowBowl # reflectX
      rimOuterLeftNarrowBowl # backward
    innerRims = do
      rimInnerNarrowBowl # reflectY
      rimInnerNarrowBowl # rotateHalfTurn # backward
      rimInnerNarrowBowl # reflectX
      rimInnerNarrowBowl # backward
    parts = do
      makePart outerRims
      makePart innerRims # backward # translate (thicknessX &| 0)
    originX = narrowBowlVirtualWidth / 2

-- x の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partXal :: Given Config => Part
partXal = parts # moveOriginBy (originX &| 0)
  where
    parts = do
      partNarrowBowl
      partNarrowBowl # reflectX # translate (narrowBowlVirtualWidth - thicknessX &| 0)
    originX = xalWidth / 2 - narrowBowlVirtualWidth / 2

nesWidth :: Given Config => Double
nesWidth = narrowBowlVirtualWidth + spineWidth

-- n の文字の書き終わりの箇所にある曲線を、上端から下端への向きで生成します。
rimNesLeg :: Given Config => Rim
rimNesLeg = origin ~> (0 &| -rightCont) ~~ zero <~ (-bend &| -height)
  where
    bend = nesLegBend
    height = mean / 2
    rightCont = height * 0.6

-- n の文字の中央部分の上側の曲線を、下端から上端への向きで生成します。
rimTopSpine :: Given Config => Rim
rimTopSpine = origin ~> (leftCont &| 0) ~~ (-rightCont &| 0) <~ (width &| bend)
  where
    width = spineWidth
    bend = mean - thicknessY + overshoot * 2
    leftCont = searchSpineInnerCont bend width rightCont
    rightCont = width * 1.05

-- n の文字の中央部分の下側の曲線を、下端から上端への向きで生成します。
rimBottomSpine :: Given Config => Rim
rimBottomSpine = origin ~> (leftCont &| 0) ~~ (-rightCont &| 0) <~ (width &| bend)
  where
    width = spineWidth
    bend = mean - thicknessY + overshoot * 2
    leftCont = width * 1.05
    rightCont = searchSpineInnerCont bend width leftCont

-- n の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partNes :: Given Config => Part
partNes = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimOuterLeftNarrowBowl # reflectY
      rimBottomSpine
      rimInnerNarrowBowl # reflectX # backward
      rimNesLeg
      rimCut
      rimNesLeg # backward
      rimOuterLeftNarrowBowl # reflectX
      rimTopSpine # backward
      rimInnerNarrowBowl # reflectY # backward
      rimNesLeg # rotateHalfTurn
      rimCut # backward
      rimNesLeg # rotateHalfTurn # backward
    originX = nesWidth / 2

-- i の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
rimLeftItTail :: Given Config => Rim
rimLeftItTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (bend &| -height)
  where
    bend = itTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = descent * 1.2
    bottomCont = searchTailInnerCont bend height topCont

-- i の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
rimRightItTail :: Given Config => Rim
rimRightItTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (bend &| -height)
  where
    bend = itTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = descent * 1.2

-- i の文字と同じ形を生成します。
-- 原点は上部の丸い部分の中央にあるので、回転や反転で変化しません。
partIt :: Given Config => Part
partIt = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimLeftItTail
      rimCut
      rimRightItTail # backward
      rimInnerBowl
      rimInnerTalBeak # backward
      rimCut
      rimOuterTalBeak
      rimOuterBowl # backward
    originX = talWidth / 2

-- u の文字のディセンダーと接続する部分の外側の曲線を、上端から下端への向きで生成します。
rimOuterLink :: Given Config => Rim
rimOuterLink = origin ~> (0 &| -leftCont) ~~ (-bottomCont &| 0) <~ (width &| -height)
  where
    width = linkWidth
    height = mean / 2 - linkLowerCorrection
    leftCont = height * 0.02
    bottomCont = width

-- u の文字のディセンダーと接続する部分の内側の曲線を、上端から下端への向きで生成します。
rimInnerLink :: Given Config => Rim
rimInnerLink = origin ~> (0 &| -leftCont) ~~ (-bottomCont &| 0) <~ (width &| -height)
  where
    width = linkWidth - thicknessX
    height = mean / 2 - thicknessY
    leftCont = height * 0.02
    bottomCont = width

-- u の文字のディセンダーの左側の曲線を、下端から上端への向きで生成します。
rimLeftUtTail :: Given Config => Rim
rimLeftUtTail = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (bend &| height)
  where
    bend = utTailBend + thicknessX / 2
    height = descent + thicknessY - linkUpperCorrection
    leftCont = height * 0.1
    topCont = bend

-- u の文字のディセンダーの右側の曲線を、下端から上端への向きで生成します。
rimRightUtTail :: Given Config => Rim
rimRightUtTail = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (bend &| height)
  where
    bend = utTailBend - thicknessX / 2
    height = descent
    leftCont = height * 0.1
    topCont = bend

-- u の文字のベースラインより上にある丸い部分を生成します。
-- ディセンダーと重ねたときに太く見えすぎないように、下側を少し細く補正してあります。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partUpperUt :: Given Config => Part
partUpperUt = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimOuterLink
      makeRim $ origin ~~ (0 &| thicknessY - linkLowerCorrection)
      rimInnerLink # backward
      rimInnerBowl
      rimInnerTalBeak # backward
      rimCut
      rimOuterTalBeak
      rimOuterBowl # backward
    originX = talWidth / 2

-- u の文字のディセンダーを生成します。
-- ベースラインより上の部分と重ねたときに太く見えすぎないように、上側を少し細く補正してあります。
-- 原点は右上の角にあります。
partUtTail :: Given Config => Part
partUtTail = makePart rims
  where
    rims = do
      rimLeftUtTail # backward
      rimCut
      rimRightUtTail
      makeRim $ origin ~~ (0 &| thicknessY - linkUpperCorrection)

-- u の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partUt :: Given Config => Part
partUt = parts
  where
    parts = do
      partUpperUt
      partUtTail # translate (-talWidth / 2 + linkWidth &| -mean / 2 + thicknessY - linkUpperCorrection)

-- 6 の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partRac :: Given Config => Part
partRac = parts
  where
    parts = do
      partYes # rotateHalfTurn
      partLesTail # translate (bowlWidth / 2 - thicknessX &| 0)

solidusLength :: Given Config => Double
solidusLength = rawLength * 2 - thicknessX
  where
    rawLength = distance origin $ head $ intersectPointsP line (head $ execMonoidState' partBowl)
    line = origin ~~ (bowlWidth / 2 &| solidusGrade)

solidusThickness :: Given Config => Double
solidusThickness = idealThickness solidusAngle * solidusThicknessRatio

solidusAngle :: Given Config => Angle Double
solidusAngle = signedAngleBetween (bowlWidth / 2 &| solidusGrade) unitX

-- 0 の文字の斜線の部分の長い方の直線を、左端から右端への向きで生成します。
-- パーツを構成した後に回転することを想定しているので、このトレイルは水平です。
rimSolidus :: Given Config => Rim
rimSolidus = origin ~~ (length &| 0)
  where
    length = solidusLength

-- 0 の文字の斜線の部分の短い方の直線を、上端から下端への向きで生成します。
-- パーツを構成した後に回転することを想定しているので、このトレイルは鉛直です。
rimSolidusCut :: Given Config => Rim
rimSolidusCut = origin ~~ (0 &| -length)
  where
    length = solidusThickness

-- 0 の文字の斜線の部分を生成します。
-- 原点は全体の中央にあります。
partSolidus :: Given Config => Part
partSolidus = makePart rims # moveOriginBy (originX &| originY) # rotate solidusAngle
  where
    rims = do
      rimSolidusCut
      rimSolidus
      rimSolidusCut # backward
      rimSolidus # backward
    originX = solidusLength / 2
    originY = -solidusThickness / 2

-- 0 の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partNuf :: Given Config => Part
partNuf = parts
  where
    parts = do
      partBowl
      partSolidus

xefHalfVirtualWidth :: Given Config => Double
xefHalfVirtualWidth = narrowBowlVirtualWidth / 2 + xefBeakWidth

xefWidth :: Given Config => Double
xefWidth = xefHalfVirtualWidth * 2 - thicknessX

-- 5 の文字の左上にある部分の外側の曲線を、右端から上端への向きで生成します。
rimOuterXefBeak :: Given Config => Rim
rimOuterXefBeak = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = xefBeakWidth
    height = xefBeakHeight + overshoot
    leftCont = height * 0.05
    topCont = width

-- 5 の文字の左上にある部分の内側の曲線を、右端から上端への向きで生成します。
rimInnerXefBeak :: Given Config => Rim
rimInnerXefBeak = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = xefBeakWidth - thicknessX
    height = xefBeakHeight - thicknessY + overshoot
    leftCont = height * 0.05
    topCont = width

-- 5 の文字の左半分を生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は全体の中央にあります。
partXefHalf :: Given Config => Part
partXefHalf = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimOuterRightNarrowBowl # reflectX
      rimOuterXefBeak # backward
      rimCut
      rimInnerXefBeak
      rimInnerNarrowBowl # reflectX # backward
      rimInnerNarrowBowl # rotateHalfTurn
      rimInnerXefBeak # reflectY # backward
      rimCut # backward
      rimOuterXefBeak # reflectY
      rimOuterRightNarrowBowl # rotateHalfTurn # backward
    originX = -xefHalfVirtualWidth / 2 + narrowBowlCorrection

-- 5 の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partXef :: Given Config => Part
partXef = parts # moveOriginBy (originX &| 0)
  where
    parts = do
      partXefHalf
      partXefHalf # reflectX # translate (xefHalfVirtualWidth - thicknessX &| 0)
    originX = xefWidth / 2 - xefHalfVirtualWidth / 2

tasWidth :: Given Config => Double
tasWidth = bowlWidth / 2 + max tasShoulderWidth tasBeakWidth

-- 1 の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
rimOuterTasBeak :: Given Config => Rim
rimOuterTasBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = tasBeakWidth
    height = tasBeakHeight + overshoot
    rightCont = height * 0.05
    topCont = width

-- 1 の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
rimInnerTasBeak :: Given Config => Rim
rimInnerTasBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = tasBeakWidth - thicknessX
    height = tasBeakHeight - thicknessY + overshoot
    rightCont = height * 0.05
    topCont = width

-- 1 の文字の右下にある部分の外側の曲線を、上端から下端への向きで生成します。
rimOuterTasShoulder :: Given Config => Rim
rimOuterTasShoulder = origin ~> (0 &| -rightCont) ~~ (bottomCont &| 0) <~ (-width &| -height)
  where
    width = tasShoulderWidth
    height = tasCrossbarAltitude + thicknessY / 2 - tasShoulderStraightHeight + overshoot
    rightCont = height * 0.1
    bottomCont = width

-- 1 の文字の右下にある部分の内側の曲線を、上端から下端への向きで生成します。
rimInnerTasShoulder :: Given Config => Rim
rimInnerTasShoulder = origin ~> (0 &| -rightCont) ~~ (bottomCont &| 0) <~ (-width &| -height)
  where
    width = tasShoulderWidth - thicknessX
    height = tasCrossbarAltitude - thicknessY / 2 - tasShoulderStraightHeight + overshoot
    rightCont = height * 0.1
    bottomCont = width

-- 1 の文字の右下にある部分に含まれる直線を、上端から下端への向きで生成します。
rimTasShoulderStraight :: Given Config => Rim
rimTasShoulderStraight = origin ~~ (0 &| height)
  where
    height = tasShoulderStraightHeight

-- 1 の文字の横線以外の部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTasFrame :: Given Config => Part
partTasFrame = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimOuterBowl # reflectY
      rimOuterTasShoulder # backward
      rimTasShoulderStraight
      rimCut # backward
      rimTasShoulderStraight # backward
      rimInnerTasShoulder
      rimInnerBowl # reflectY # backward
      rimInnerBowl
      rimInnerTasBeak # backward
      rimCut
      rimOuterTasBeak
      rimOuterBowl # backward
    originX = tasWidth / 2

-- 1 の文字の横線の部分の直線を、左端から右端への向きで生成します。
rimTasCrossbar :: Given Config => Rim
rimTasCrossbar = origin ~~ (width &| 0)
  where
    width = bowlWidth / 2 + tasShoulderWidth - thicknessX

-- 文字の書き始めや書き終わりの位置にある垂直に切られた部分を、上端から下端への向きで生成します。
rimVerticalCut :: Given Config => Rim
rimVerticalCut = origin ~~ (0 &| -height)
  where
    height = thicknessY

-- 1 の文字の横線の部分を生成します。
-- 原点は左上の角にあります。
partTasCrossbar :: Given Config => Part
partTasCrossbar = makePart rims
  where
    rims = do
      rimVerticalCut
      rimTasCrossbar
      rimVerticalCut # backward
      rimTasCrossbar # backward

-- 1 の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partTas :: Given Config => Part
partTas = parts
  where
    parts = do
      partTasFrame
      partTasCrossbar # translate (thicknessX / 2 - tasWidth / 2 &| tasCrossbarAltitude - mean / 2 + thicknessY / 2)

-- 3 の文字の左上にある丸い部分の外側の曲線を、左端から上端への向きで生成します。
rimOuterYusBowl :: Given Config => Rim
rimOuterYusBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = yusWidth / 2
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- 3 の文字の左上にある丸い部分の内側の曲線を、左端から上端への向きで生成します。
rimInnerYusBowl :: Given Config => Rim
rimInnerYusBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = yusWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot
    leftCont = height * 0.1
    topCont = width

-- 3 の文字の右下にある曲線を、上端から下端への向きで生成します。
rimYusLeg :: Given Config => Rim
rimYusLeg = origin ~> (0 &| -leftCont) ~~ zero <~ (-bend &| -height)
  where
    bend = yusLegBend
    height = mean / 2
    leftCont = height * 0.6

-- 3 の文字の左下にある部分の外側の曲線を、左端から下端への向きで生成します。
rimOuterYusShoulder :: Given Config => Rim
rimOuterYusShoulder = origin ~> (0 &| -leftCont) ~~ (-topCont &| 0) <~ (width &| -height)
  where
    width = yusCrossbarLatitude + thicknessX * yusCrossbarThicknessRatio / 2 - yusShoulderStraightWidth
    height = mean / 2
    leftCont = height * 0.1
    topCont = width

-- 3 の文字の左下にある部分の内側の曲線を、左端から下端への向きで生成します。
rimInnerYusShoulder :: Given Config => Rim
rimInnerYusShoulder = origin ~> (0 &| -leftCont) ~~ (-topCont &| 0) <~ (width &| -height)
  where
    width = yusCrossbarLatitude + thicknessX * (yusCrossbarThicknessRatio - 2) / 2 - yusShoulderStraightWidth
    height = mean / 2 - thicknessY
    leftCont = height * 0.1
    topCont = width

-- 3 の文字の左下にある部分に含まれる直線を、左端から右端への向きで生成します。
rimYusShoulderStraight :: Given Config => Rim
rimYusShoulderStraight = origin ~~ (width &| 0)
  where
    width = yusShoulderStraightWidth

-- 3 の文字の縦線以外の部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partYusFrame :: Given Config => Part
partYusFrame = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimOuterYusShoulder
      rimYusShoulderStraight
      rimVerticalCut # backward
      rimYusShoulderStraight # backward
      rimInnerYusShoulder # backward
      rimInnerYusBowl
      rimInnerYusBowl # reflectX # backward
      rimYusLeg
      rimCut
      rimYusLeg # backward
      rimOuterYusBowl # reflectX
      rimOuterYusBowl # backward
    originX = yusWidth / 2

-- 3 の文字の縦線の部分の直線を、上端から下端への向きで生成します。
rimYusCrossbar :: Given Config => Rim
rimYusCrossbar = origin ~~ (0 &| -height)
  where
    height = mean - thicknessY

-- 3 の文字の縦線の部分の水平に切られた部分を、左端から右端への向きで生成します。
rimYusCrossbarCut :: Given Config => Rim
rimYusCrossbarCut = origin ~~ (width &| 0)
  where
    width = thicknessX * yusCrossbarThicknessRatio

-- 3 の文字と縦線の部分を生成します。
-- 原点は左上の角にあります。
partYusCrossbar :: Given Config => Part
partYusCrossbar = makePart rims
  where
    rims = do
      rimYusCrossbar
      rimYusCrossbarCut
      rimYusCrossbar # backward
      rimYusCrossbarCut # backward

-- 3 の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partYus :: Given Config => Part
partYus = parts
  where
    parts = do
      partYusFrame
      partYusCrossbar # translate (yusCrossbarLatitude - yusWidth / 2 - thicknessX * yusCrossbarThicknessRatio / 2 &| mean / 2 - thicknessY / 2)

-- 変音符の右に飛び出るように曲がる曲線の上半分を、下端から上端への向きで生成します。
rimTransphone :: Given Config => Rim
rimTransphone = origin ~> zero ~~ (0 &| rightCont) <~ (bend &| -height)
  where
    bend = transphoneBend
    height = mean / 2
    rightCont = height * 0.6

-- 変音符の上下にある水平に切られた部分を、左端から右端への向きで生成します。
rimTransphoneCut :: Given Config => Rim
rimTransphoneCut = origin ~~ (width &| 0)
  where
    width = thicknessX * transphoneThicknessRatio

-- 変音符と同じ形を生成します。
-- 原点は右に飛び出る部分の左中央にあります。
partTransphone :: Given Config => Part
partTransphone = makePart rims # moveOriginBy (originX &| originY)
  where
    rims = do
      rimTransphone
      rimTransphone # reflectY # backward
      rimTransphoneCut
      rimTransphone # reflectY
      rimTransphone # backward
      rimTransphoneCut # backward
    originX = transphoneBend
    originY = -mean / 2
  
-- アキュートアクセントの丸い部分の外側の曲線の半分を、左下端から上端への向きで生成します。
rimOuterAcute :: Given Config => Rim
rimOuterAcute = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = acuteWidth / 2
    height = acuteHeight
    leftCont = height * 0.1
    topCont = width
    
-- アキュートアクセントの丸い部分の内側の曲線の半分を、左下端から上端への向きで生成します。
rimInnerAcute :: Given Config => Rim
rimInnerAcute = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = acuteWidth / 2 - acuteThicknessX
    height = acuteHeight - acuteThicknessY
    leftCont = height * 0.1
    topCont = width

-- アキュートアクセントの下部にある水平に切られた部分を、左端から右端への向きで生成します。
rimAcuteCut :: Given Config => Rim
rimAcuteCut = origin ~~ (width &| 0)
  where
    width = acuteThicknessX

-- アキュートアクセントと同じ形を生成します。
-- 原点は下部中央にあります。
partAcute :: Given Config => Part
partAcute = makePart rims # moveOriginBy (originX &| 0)
  where
    rims = do
      rimAcuteCut
      rimInnerAcute
      rimInnerAcute # reflectX # backward
      rimAcuteCut
      rimOuterAcute # reflectX
      rimOuterAcute # backward
    originX = acuteWidth / 2

-- サーカムフレックスアクセントの外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
rimOuterCircumflex :: Given Config => Rim
rimOuterCircumflex = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = circumflexWidth / 2
    height = circumflexHeight / 2
    leftCont = height * 0.1
    topCont = width

-- サーカムフレックスアクセントの内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
rimInnerCircumflex :: Given Config => Rim
rimInnerCircumflex = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = circumflexWidth / 2 - circumflexThicknessX
    height = circumflexHeight / 2 - circumflexThicknessY
    leftCont = height * 0.1
    topCont = width

-- サーカムフレックスアクセントと同じ形を生成します。
-- 原点は下部中央にあります。
partCircumflex :: Given Config => Part
partCircumflex = concatPart parts # moveOriginBy (originX &| originY)
  where
    outerRims = do
      rimOuterCircumflex # reflectY
      rimOuterCircumflex # rotateHalfTurn # backward
      rimOuterCircumflex # reflectX
      rimOuterCircumflex # backward
    innerRims = do
      rimInnerCircumflex # reflectY
      rimInnerCircumflex # rotateHalfTurn # backward
      rimInnerCircumflex # reflectX
      rimInnerCircumflex # backward
    parts = do
      makePart outerRims
      makePart innerRims # backward # translate (circumflexThicknessX &| 0)
    originX = circumflexWidth / 2
    originY = -circumflexHeight / 2

-- デックやパデックなどに含まれる円の曲線を、左端から反時計回りに生成します。
rimDot :: Given Config => Rim
rimDot = circle radius # rotateHalfTurn
  where
    radius = dotWidth / 2

-- デックやパデックなどに含まれる円を生成します。
-- 原点は円に外接する矩形の左下の角からオーバーシュート分だけ上に移動した位置にあります。
partDot :: Given Config => Part
partDot = makePart rims # moveOriginBy (0 &| originY)
  where
    rims = do
      rimDot
    originY = -dotWidth / 2 + overshoot

-- カルタックなどに含まれるベースラインより上に浮いた円を生成します。
-- partDot が返すパーツと形は同じですが、原点の位置が異なります。
-- 原点は左端にあります。
partFloatingDot :: Given Config => Part
partFloatingDot = makePart rims
  where
    rims = do
      rimDot

-- バデックの棒状の部分の直線を、上端から下端への向きで生成します。
rimBadekStem :: Given Config => Rim
rimBadekStem = origin ~~ (0 &| -height)
  where
    height = ascent - dotWidth - badekGap + overshoot

-- バデックの棒状の部分を生成します。
-- 原点は左下の角にあります。
partBadekStem :: Given Config => Part
partBadekStem = makePart rims
  where
    rims = do
      rimCut
      rimBadekStem # backward
      rimCut # backward
      rimBadekStem

-- パデックの棒状の部分の左側の曲線を、上端から下端への向きで生成します。
rimLeftPadekStem :: Given Config => Rim
rimLeftPadekStem = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = padekBend
    height = ascent - dotWidth - badekGap + overshoot
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = height * 0.55

-- パデックの棒状の部分の右側の曲線を、上端から下端への向きで生成します。
rimRightPadekStem :: Given Config => Rim
rimRightPadekStem = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = padekBend
    height = ascent - dotWidth - badekGap + overshoot
    topCont = height * 0.55
    bottomCont = searchTailInnerCont bend height topCont

-- パデックの棒状の部分を生成します。
-- 原点は左下の角にあります。
partPadekStem :: Given Config => Part
partPadekStem = makePart rims
  where
    rims = do
      rimCut
      rimRightPadekStem # backward
      rimCut # backward
      rimLeftPadekStem

-- ノークの棒状の部分の縦の曲線を、上端から下端への向きで生成します。
rimNokStem :: Given Config => Rim
rimNokStem = origin ~~ (0 &| -height)
  where
    height = nokHeight

-- ノークと同じ形を生成します。
-- 原点は左上の角にあります。
partNok :: Given Config => Part
partNok = makePart rims
  where
    rims = do
      rimNokStem
      rimCut
      rimNokStem # backward
      rimCut # backward

-- ディカックの棒状の部分の曲線を、上端から下端への向きで生成します。
rimDikakStem :: Given Config => Rim
rimDikakStem = origin ~> zero ~~ (0 &| leftCont) <~ (-bend &| -height)
  where
    bend = dikakBend
    height = dikakHeight
    leftCont = height * 0.6

-- ディカックと同じ形を生成します。
-- 原点は左上の角にあります。
partDikak :: Given Config => Part
partDikak = makePart rims
  where
    rims = do
      rimDikakStem
      rimCut
      rimDikakStem # backward
      rimCut # backward

-- フェークの直線を、左端から右端への向きで生成します。
rimFekHorizontal :: Given Config => Rim
rimFekHorizontal = origin ~~ (width &| 0)
  where
    width = fekWidth

-- フェークと同じ形を生成します。
-- 原点は左上の角にあります。
partFek :: Given Config => Part
partFek = makePart rims
  where
    rims = do
      rimVerticalCut
      rimFekHorizontal
      rimVerticalCut # backward
      rimFekHorizontal # backward

-- フォーハックの直線を、左端から右端への向きで生成します。
rimFohakHorizontal :: Given Config => Rim
rimFohakHorizontal = origin ~~ (width &| 0)
  where
    width = fohakWidth

-- フォーハックと同じ形を生成します。
-- 原点は左上の角にあります。
partFohak :: Given Config => Part
partFohak = makePart rims
  where
    rims = do
      rimVerticalCut
      rimFohakHorizontal
      rimVerticalCut # backward
      rimFohakHorizontal # backward

-- ダッシュの直線を、左端から右端への向きで生成します。
rimDashHorizontal :: Given Config => Rim
rimDashHorizontal = origin ~~ (width &| 0)
  where
    width = dashWidth

-- ダッシュと同じ形を生成します。
-- 原点は左上の角にあります。
partDash :: Given Config => Part
partDash = makePart rims
  where
    rims = do
      rimVerticalCut
      rimDashHorizontal
      rimVerticalCut # backward
      rimDashHorizontal # backward

-- ラクットの縦向きの棒状の部分の直線を、上端から下端への向きで生成します。
rimRakutVertical :: Given Config => Rim
rimRakutVertical = origin ~~ (0 &| -height)
  where
    height = rakutHeight

-- ラクットの横向きの棒状の部分の直線を、左端から右端への向きで生成します。
rimRakutHorizontal :: Given Config => Rim
rimRakutHorizontal = origin ~~ (width &| 0)
  where
    width = rakutWidth

-- ラクットの縦向きの棒状の部分を生成します。
-- 原点は左上の角にあります。
partRakutVertical :: Given Config => Part
partRakutVertical = makePart rims
  where
    rims = do
      rimRakutVertical
      rimCut
      rimRakutVertical # backward
      rimCut # backward

-- ラクットの横向きの棒状の部分を生成します。
-- 原点は左上の角にあります。
partRakutHorizontal :: Given Config => Part
partRakutHorizontal = makePart rims
  where
    rims = do
      rimVerticalCut
      rimRakutHorizontal
      rimVerticalCut # backward
      rimRakutHorizontal # backward

-- 開きラクットと同じ形を生成します。
-- 原点は左上の角にあります。
partOpeningRakut :: Given Config => Part
partOpeningRakut = parts
  where
    parts = do
      partRakutVertical
      partRakutHorizontal

ascent :: Given Config => Double
ascent = mean + descent

em :: Given Config => Double
em = ascent + descent

actualDescent :: Given Config => Double
actualDescent = descent + extraDescent

actualAscent :: Given Config => Double
actualAscent = ascent + extraAscent

actualEm :: Given Config => Double
actualEm = actualDescent + actualAscent