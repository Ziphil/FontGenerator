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
boneOuterBowl :: Given Config => Bone
boneOuterBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = bowlWidth / 2
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- k, p, c, l, a などの文字に共通する丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
boneInnerBowl :: Given Config => Bone
boneInnerBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = bowlWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot
    leftCont = height * 0.1
    topCont = width

-- k, p, c, l, a などの文字に共通する丸い部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partBowl :: Given Config => Part
partBowl = unite parts #.~> (originX &| 0)
  where
    outerBones = do
      boneOuterBowl # reflectY
      boneOuterBowl # rotateHalfTurn # backward
      boneOuterBowl # reflectX
      boneOuterBowl # backward
    innerBones = do
      boneInnerBowl # reflectY
      boneInnerBowl # rotateHalfTurn # backward
      boneInnerBowl # reflectX
      boneInnerBowl # backward
    parts = do
      partBy outerBones
      partBy innerBones # backward # translate (thicknessX &| 0)
    originX = bowlWidth / 2

-- l の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
boneLeftLesTail :: Given Config => Bone
boneLeftLesTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = lesTailBend - thicknessX / 2 + lesTailCorrection
    virtualBend = lesTailBend
    height = mean / 2 + descent
    topCont = searchTailInnerCont virtualBend height bottomCont
    bottomCont = descent * 1.08

-- l の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
boneRightLesTail :: Given Config => Bone
boneRightLesTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = lesTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = descent * 1.08
    bottomCont = searchTailInnerCont bend height topCont

-- 文字の書き始めや書き終わりの位置にある水平に切られた部分を、左端から右端への向きで生成します。
boneCut :: Given Config => Bone
boneCut = origin ~~ (width &| 0)
  where
    width = thicknessX

-- l の文字のディセンダーを生成します。
-- 反転や回転を施すことで、c などの文字のディセンダーや k, p などの文字のアセンダーとしても使えます。
-- 丸い部分と重ねたときに重なった部分が太く見えすぎないように、左側を少し細く補正してあります。
-- 原点は補正がないとしたときの左上の角にあります。
partLesTail :: Given Config => Part
partLesTail = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneLeftLesTail
      boneCut
      boneRightLesTail # backward
      boneBy $ origin ~~ (-thicknessX + lesTailCorrection &| 0)
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
boneYesLeg :: Given Config => Bone
boneYesLeg = origin ~> (0 &| -leftCont) ~~ zero <~ (bend &| -height)
  where
    bend = yesLegBend
    height = mean / 2
    leftCont = height * 0.6

-- y の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partYes :: Given Config => Part
partYes = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneYesLeg
      boneCut
      boneYesLeg # backward
      boneInnerBowl
      boneInnerBowl # reflectX # backward
      boneYesLeg # reflectX
      boneCut
      boneYesLeg # reflectX # backward
      boneOuterBowl # reflectX
      boneOuterBowl # backward
    originX = bowlWidth / 2

talWidth :: Given Config => Double
talWidth = bowlWidth / 2 + talBeakWidth

-- t の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
boneOuterTalBeak :: Given Config => Bone
boneOuterTalBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = talBeakWidth
    height = talBeakHeight + overshoot
    rightCont = height * 0.05
    topCont = width

-- t の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
boneInnerTalBeak :: Given Config => Bone
boneInnerTalBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = talBeakWidth - thicknessX
    height = talBeakHeight - thicknessY + overshoot
    rightCont = height * 0.05
    topCont = width

-- t の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTal :: Given Config => Part
partTal = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneOuterBowl # reflectY
      boneOuterTalBeak # reflectY # backward
      boneCut # backward
      boneInnerTalBeak # reflectY
      boneInnerBowl # reflectY # backward
      boneInnerBowl
      boneInnerTalBeak # backward
      boneCut
      boneOuterTalBeak
      boneOuterBowl # backward
    originX = talWidth / 2

narrowBowlWidth :: Given Config => Double
narrowBowlWidth = narrowBowlVirtualWidth - narrowBowlCorrection

xalWidth :: Given Config => Double
xalWidth = narrowBowlVirtualWidth * 2 - thicknessX

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
boneOuterLeftNarrowBowl :: Given Config => Bone
boneOuterLeftNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、右端から上端への向きで生成します。
-- ただし、他のトレイルと使い方を揃えるため、左右反転してあります。
boneOuterRightNarrowBowl :: Given Config => Bone
boneOuterRightNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
boneInnerNarrowBowl :: Given Config => Bone
boneInnerNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分を生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は全体の中央にあります。
partNarrowBowl :: Given Config => Part
partNarrowBowl = unite parts #.~> (originX &| 0)
  where
    outerBones = do
      boneOuterLeftNarrowBowl # reflectY
      boneOuterRightNarrowBowl # rotateHalfTurn # backward
      boneOuterRightNarrowBowl # reflectX
      boneOuterLeftNarrowBowl # backward
    innerBones = do
      boneInnerNarrowBowl # reflectY
      boneInnerNarrowBowl # rotateHalfTurn # backward
      boneInnerNarrowBowl # reflectX
      boneInnerNarrowBowl # backward
    parts = do
      partBy outerBones
      partBy innerBones # backward # translate (thicknessX &| 0)
    originX = narrowBowlVirtualWidth / 2

-- x の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partXal :: Given Config => Part
partXal = parts #.~> (originX &| 0)
  where
    parts = do
      partNarrowBowl
      partNarrowBowl # reflectX # translate (narrowBowlVirtualWidth - thicknessX &| 0)
    originX = xalWidth / 2 - narrowBowlVirtualWidth / 2

nesWidth :: Given Config => Double
nesWidth = narrowBowlVirtualWidth + spineWidth

-- n の文字の書き終わりの箇所にある曲線を、上端から下端への向きで生成します。
boneNesLeg :: Given Config => Bone
boneNesLeg = origin ~> (0 &| -rightCont) ~~ zero <~ (-bend &| -height)
  where
    bend = nesLegBend
    height = mean / 2
    rightCont = height * 0.6

-- n の文字の中央部分の上側の曲線を、下端から上端への向きで生成します。
boneTopSpine :: Given Config => Bone
boneTopSpine = origin ~> (leftCont &| 0) ~~ (-rightCont &| 0) <~ (width &| bend)
  where
    width = spineWidth
    bend = mean - thicknessY + overshoot * 2
    leftCont = searchSpineInnerCont bend width rightCont
    rightCont = width * 1.05

-- n の文字の中央部分の下側の曲線を、下端から上端への向きで生成します。
boneBottomSpine :: Given Config => Bone
boneBottomSpine = origin ~> (leftCont &| 0) ~~ (-rightCont &| 0) <~ (width &| bend)
  where
    width = spineWidth
    bend = mean - thicknessY + overshoot * 2
    leftCont = width * 1.05
    rightCont = searchSpineInnerCont bend width leftCont

-- n の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partNes :: Given Config => Part
partNes = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneOuterLeftNarrowBowl # reflectY
      boneBottomSpine
      boneInnerNarrowBowl # reflectX # backward
      boneNesLeg
      boneCut
      boneNesLeg # backward
      boneOuterLeftNarrowBowl # reflectX
      boneTopSpine # backward
      boneInnerNarrowBowl # reflectY # backward
      boneNesLeg # rotateHalfTurn
      boneCut # backward
      boneNesLeg # rotateHalfTurn # backward
    originX = nesWidth / 2

-- i の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
boneLeftItTail :: Given Config => Bone
boneLeftItTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (bend &| -height)
  where
    bend = itTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = descent * 1.2
    bottomCont = searchTailInnerCont bend height topCont

-- i の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
boneRightItTail :: Given Config => Bone
boneRightItTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (bend &| -height)
  where
    bend = itTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = descent * 1.2

-- i の文字と同じ形を生成します。
-- 原点は上部の丸い部分の中央にあるので、回転や反転で変化しません。
partIt :: Given Config => Part
partIt = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneLeftItTail
      boneCut
      boneRightItTail # backward
      boneInnerBowl
      boneInnerTalBeak # backward
      boneCut
      boneOuterTalBeak
      boneOuterBowl # backward
    originX = talWidth / 2

-- u の文字のディセンダーと接続する部分の外側の曲線を、上端から下端への向きで生成します。
boneOuterLink :: Given Config => Bone
boneOuterLink = origin ~> (0 &| -leftCont) ~~ (-bottomCont &| 0) <~ (width &| -height)
  where
    width = linkWidth
    height = mean / 2 - linkLowerCorrection
    leftCont = height * 0.02
    bottomCont = width

-- u の文字のディセンダーと接続する部分の内側の曲線を、上端から下端への向きで生成します。
boneInnerLink :: Given Config => Bone
boneInnerLink = origin ~> (0 &| -leftCont) ~~ (-bottomCont &| 0) <~ (width &| -height)
  where
    width = linkWidth - thicknessX
    height = mean / 2 - thicknessY
    leftCont = height * 0.02
    bottomCont = width

-- u の文字のディセンダーの左側の曲線を、下端から上端への向きで生成します。
boneLeftUtTail :: Given Config => Bone
boneLeftUtTail = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (bend &| height)
  where
    bend = utTailBend + thicknessX / 2
    height = descent + thicknessY - linkUpperCorrection
    leftCont = height * 0.1
    topCont = bend

-- u の文字のディセンダーの右側の曲線を、下端から上端への向きで生成します。
boneRightUtTail :: Given Config => Bone
boneRightUtTail = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (bend &| height)
  where
    bend = utTailBend - thicknessX / 2
    height = descent
    leftCont = height * 0.1
    topCont = bend

-- u の文字のベースラインより上にある丸い部分を生成します。
-- ディセンダーと重ねたときに太く見えすぎないように、下側を少し細く補正してあります。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partUpperUt :: Given Config => Part
partUpperUt = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneOuterLink
      boneBy $ origin ~~ (0 &| thicknessY - linkLowerCorrection)
      boneInnerLink # backward
      boneInnerBowl
      boneInnerTalBeak # backward
      boneCut
      boneOuterTalBeak
      boneOuterBowl # backward
    originX = talWidth / 2

-- u の文字のディセンダーを生成します。
-- ベースラインより上の部分と重ねたときに太く見えすぎないように、上側を少し細く補正してあります。
-- 原点は右上の角にあります。
partUtTail :: Given Config => Part
partUtTail = partBy bones
  where
    bones = do
      boneLeftUtTail # backward
      boneCut
      boneRightUtTail
      boneBy $ origin ~~ (0 &| thicknessY - linkUpperCorrection)

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
    rawLength = distance origin $ head $ intersectPointsP line (head $ execState' partBowl)
    line = origin ~~ (bowlWidth / 2 &| solidusGrade)

solidusThickness :: Given Config => Double
solidusThickness = idealThickness solidusAngle * solidusThicknessRatio

solidusAngle :: Given Config => Angle Double
solidusAngle = signedAngleBetween (bowlWidth / 2 &| solidusGrade) unitX

-- 0 の文字の斜線の部分の長い方の直線を、左端から右端への向きで生成します。
-- パーツを構成した後に回転することを想定しているので、このトレイルは水平です。
boneSolidus :: Given Config => Bone
boneSolidus = origin ~~ (length &| 0)
  where
    length = solidusLength

-- 0 の文字の斜線の部分の短い方の直線を、上端から下端への向きで生成します。
-- パーツを構成した後に回転することを想定しているので、このトレイルは鉛直です。
boneSolidusCut :: Given Config => Bone
boneSolidusCut = origin ~~ (0 &| -length)
  where
    length = solidusThickness

-- 0 の文字の斜線の部分を生成します。
-- 原点は全体の中央にあります。
partSolidus :: Given Config => Part
partSolidus = partBy bones #.~> (originX &| originY) # rotate solidusAngle
  where
    bones = do
      boneSolidusCut
      boneSolidus
      boneSolidusCut # backward
      boneSolidus # backward
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
boneOuterXefBeak :: Given Config => Bone
boneOuterXefBeak = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = xefBeakWidth
    height = xefBeakHeight + overshoot
    leftCont = height * 0.05
    topCont = width

-- 5 の文字の左上にある部分の内側の曲線を、右端から上端への向きで生成します。
boneInnerXefBeak :: Given Config => Bone
boneInnerXefBeak = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = xefBeakWidth - thicknessX
    height = xefBeakHeight - thicknessY + overshoot
    leftCont = height * 0.05
    topCont = width

-- 5 の文字の左半分を生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は全体の中央にあります。
partXefHalf :: Given Config => Part
partXefHalf = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneOuterRightNarrowBowl # reflectX
      boneOuterXefBeak # backward
      boneCut
      boneInnerXefBeak
      boneInnerNarrowBowl # reflectX # backward
      boneInnerNarrowBowl # rotateHalfTurn
      boneInnerXefBeak # reflectY # backward
      boneCut # backward
      boneOuterXefBeak # reflectY
      boneOuterRightNarrowBowl # rotateHalfTurn # backward
    originX = -xefHalfVirtualWidth / 2 + narrowBowlCorrection

-- 5 の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partXef :: Given Config => Part
partXef = parts #.~> (originX &| 0)
  where
    parts = do
      partXefHalf
      partXefHalf # reflectX # translate (xefHalfVirtualWidth - thicknessX &| 0)
    originX = xefWidth / 2 - xefHalfVirtualWidth / 2

tasWidth :: Given Config => Double
tasWidth = bowlWidth / 2 + max tasShoulderWidth tasBeakWidth

-- 1 の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
boneOuterTasBeak :: Given Config => Bone
boneOuterTasBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = tasBeakWidth
    height = tasBeakHeight + overshoot
    rightCont = height * 0.05
    topCont = width

-- 1 の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
boneInnerTasBeak :: Given Config => Bone
boneInnerTasBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = tasBeakWidth - thicknessX
    height = tasBeakHeight - thicknessY + overshoot
    rightCont = height * 0.05
    topCont = width

-- 1 の文字の右下にある部分の外側の曲線を、上端から下端への向きで生成します。
boneOuterTasShoulder :: Given Config => Bone
boneOuterTasShoulder = origin ~> (0 &| -rightCont) ~~ (bottomCont &| 0) <~ (-width &| -height)
  where
    width = tasShoulderWidth
    height = tasCrossbarAltitude + thicknessY / 2 - tasShoulderStraightHeight + overshoot
    rightCont = height * 0.1
    bottomCont = width

-- 1 の文字の右下にある部分の内側の曲線を、上端から下端への向きで生成します。
boneInnerTasShoulder :: Given Config => Bone
boneInnerTasShoulder = origin ~> (0 &| -rightCont) ~~ (bottomCont &| 0) <~ (-width &| -height)
  where
    width = tasShoulderWidth - thicknessX
    height = tasCrossbarAltitude - thicknessY / 2 - tasShoulderStraightHeight + overshoot
    rightCont = height * 0.1
    bottomCont = width

-- 1 の文字の右下にある部分に含まれる直線を、上端から下端への向きで生成します。
boneTasShoulderStraight :: Given Config => Bone
boneTasShoulderStraight = origin ~~ (0 &| height)
  where
    height = tasShoulderStraightHeight

-- 1 の文字の横線以外の部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTasFrame :: Given Config => Part
partTasFrame = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneOuterBowl # reflectY
      boneOuterTasShoulder # backward
      boneTasShoulderStraight
      boneCut # backward
      boneTasShoulderStraight # backward
      boneInnerTasShoulder
      boneInnerBowl # reflectY # backward
      boneInnerBowl
      boneInnerTasBeak # backward
      boneCut
      boneOuterTasBeak
      boneOuterBowl # backward
    originX = tasWidth / 2

-- 1 の文字の横線の部分の直線を、左端から右端への向きで生成します。
boneTasCrossbar :: Given Config => Bone
boneTasCrossbar = origin ~~ (width &| 0)
  where
    width = bowlWidth / 2 + tasShoulderWidth - thicknessX

-- 文字の書き始めや書き終わりの位置にある垂直に切られた部分を、上端から下端への向きで生成します。
boneVerticalCut :: Given Config => Bone
boneVerticalCut = origin ~~ (0 &| -height)
  where
    height = thicknessY

-- 1 の文字の横線の部分を生成します。
-- 原点は左上の角にあります。
partTasCrossbar :: Given Config => Part
partTasCrossbar = partBy bones
  where
    bones = do
      boneVerticalCut
      boneTasCrossbar
      boneVerticalCut # backward
      boneTasCrossbar # backward

-- 1 の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partTas :: Given Config => Part
partTas = parts
  where
    parts = do
      partTasFrame
      partTasCrossbar # translate (thicknessX / 2 - tasWidth / 2 &| tasCrossbarAltitude - mean / 2 + thicknessY / 2)

-- 3 の文字の左上にある丸い部分の外側の曲線を、左端から上端への向きで生成します。
boneOuterYusBowl :: Given Config => Bone
boneOuterYusBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = yusWidth / 2
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- 3 の文字の左上にある丸い部分の内側の曲線を、左端から上端への向きで生成します。
boneInnerYusBowl :: Given Config => Bone
boneInnerYusBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = yusWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot
    leftCont = height * 0.1
    topCont = width

-- 3 の文字の右下にある曲線を、上端から下端への向きで生成します。
boneYusLeg :: Given Config => Bone
boneYusLeg = origin ~> (0 &| -leftCont) ~~ zero <~ (-bend &| -height)
  where
    bend = yusLegBend
    height = mean / 2
    leftCont = height * 0.6

-- 3 の文字の左下にある部分の外側の曲線を、左端から下端への向きで生成します。
boneOuterYusShoulder :: Given Config => Bone
boneOuterYusShoulder = origin ~> (0 &| -leftCont) ~~ (-topCont &| 0) <~ (width &| -height)
  where
    width = yusCrossbarLatitude + thicknessX * yusCrossbarThicknessRatio / 2 - yusShoulderStraightWidth
    height = mean / 2
    leftCont = height * 0.1
    topCont = width

-- 3 の文字の左下にある部分の内側の曲線を、左端から下端への向きで生成します。
boneInnerYusShoulder :: Given Config => Bone
boneInnerYusShoulder = origin ~> (0 &| -leftCont) ~~ (-topCont &| 0) <~ (width &| -height)
  where
    width = yusCrossbarLatitude + thicknessX * (yusCrossbarThicknessRatio - 2) / 2 - yusShoulderStraightWidth
    height = mean / 2 - thicknessY
    leftCont = height * 0.1
    topCont = width

-- 3 の文字の左下にある部分に含まれる直線を、左端から右端への向きで生成します。
boneYusShoulderStraight :: Given Config => Bone
boneYusShoulderStraight = origin ~~ (width &| 0)
  where
    width = yusShoulderStraightWidth

-- 3 の文字の縦線以外の部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partYusFrame :: Given Config => Part
partYusFrame = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneOuterYusShoulder
      boneYusShoulderStraight
      boneVerticalCut # backward
      boneYusShoulderStraight # backward
      boneInnerYusShoulder # backward
      boneInnerYusBowl
      boneInnerYusBowl # reflectX # backward
      boneYusLeg
      boneCut
      boneYusLeg # backward
      boneOuterYusBowl # reflectX
      boneOuterYusBowl # backward
    originX = yusWidth / 2

-- 3 の文字の縦線の部分の直線を、上端から下端への向きで生成します。
boneYusCrossbar :: Given Config => Bone
boneYusCrossbar = origin ~~ (0 &| -height)
  where
    height = mean - thicknessY

-- 3 の文字の縦線の部分の水平に切られた部分を、左端から右端への向きで生成します。
boneYusCrossbarCut :: Given Config => Bone
boneYusCrossbarCut = origin ~~ (width &| 0)
  where
    width = thicknessX * yusCrossbarThicknessRatio

-- 3 の文字と縦線の部分を生成します。
-- 原点は左上の角にあります。
partYusCrossbar :: Given Config => Part
partYusCrossbar = partBy bones
  where
    bones = do
      boneYusCrossbar
      boneYusCrossbarCut
      boneYusCrossbar # backward
      boneYusCrossbarCut # backward

-- 3 の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partYus :: Given Config => Part
partYus = parts
  where
    parts = do
      partYusFrame
      partYusCrossbar # translate (yusCrossbarLatitude - yusWidth / 2 - thicknessX * yusCrossbarThicknessRatio / 2 &| mean / 2 - thicknessY / 2)

-- 変音符の右に飛び出るように曲がる曲線の上半分を、下端から上端への向きで生成します。
boneTransphone :: Given Config => Bone
boneTransphone = origin ~> zero ~~ (0 &| rightCont) <~ (bend &| -height)
  where
    bend = transphoneBend
    height = mean / 2
    rightCont = height * 0.6

-- 変音符の上下にある水平に切られた部分を、左端から右端への向きで生成します。
boneTransphoneCut :: Given Config => Bone
boneTransphoneCut = origin ~~ (width &| 0)
  where
    width = thicknessX * transphoneThicknessRatio

-- 変音符と同じ形を生成します。
-- 原点は右に飛び出る部分の左中央にあります。
partTransphone :: Given Config => Part
partTransphone = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneTransphone
      boneTransphone # reflectY # backward
      boneTransphoneCut
      boneTransphone # reflectY
      boneTransphone # backward
      boneTransphoneCut # backward
    originX = transphoneBend
    originY = -mean / 2
  
-- アキュートアクセントの丸い部分の外側の曲線の半分を、左下端から上端への向きで生成します。
boneOuterAcute :: Given Config => Bone
boneOuterAcute = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = acuteWidth / 2
    height = acuteHeight
    leftCont = height * 0.1
    topCont = width
    
-- アキュートアクセントの丸い部分の内側の曲線の半分を、左下端から上端への向きで生成します。
boneInnerAcute :: Given Config => Bone
boneInnerAcute = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = acuteWidth / 2 - acuteThicknessX
    height = acuteHeight - acuteThicknessY
    leftCont = height * 0.1
    topCont = width

-- アキュートアクセントの下部にある水平に切られた部分を、左端から右端への向きで生成します。
boneAcuteCut :: Given Config => Bone
boneAcuteCut = origin ~~ (width &| 0)
  where
    width = acuteThicknessX

-- アキュートアクセントと同じ形を生成します。
-- 原点は下部中央にあります。
partAcute :: Given Config => Part
partAcute = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneAcuteCut
      boneInnerAcute
      boneInnerAcute # reflectX # backward
      boneAcuteCut
      boneOuterAcute # reflectX
      boneOuterAcute # backward
    originX = acuteWidth / 2

-- サーカムフレックスアクセントの外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
boneOuterCircumflex :: Given Config => Bone
boneOuterCircumflex = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = circumflexWidth / 2
    height = circumflexHeight / 2
    leftCont = height * 0.1
    topCont = width

-- サーカムフレックスアクセントの内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
boneInnerCircumflex :: Given Config => Bone
boneInnerCircumflex = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = circumflexWidth / 2 - circumflexThicknessX
    height = circumflexHeight / 2 - circumflexThicknessY
    leftCont = height * 0.1
    topCont = width

-- サーカムフレックスアクセントと同じ形を生成します。
-- 原点は下部中央にあります。
partCircumflex :: Given Config => Part
partCircumflex = unite parts #.~> (originX &| originY)
  where
    outerBones = do
      boneOuterCircumflex # reflectY
      boneOuterCircumflex # rotateHalfTurn # backward
      boneOuterCircumflex # reflectX
      boneOuterCircumflex # backward
    innerBones = do
      boneInnerCircumflex # reflectY
      boneInnerCircumflex # rotateHalfTurn # backward
      boneInnerCircumflex # reflectX
      boneInnerCircumflex # backward
    parts = do
      partBy outerBones
      partBy innerBones # backward # translate (circumflexThicknessX &| 0)
    originX = circumflexWidth / 2
    originY = -circumflexHeight / 2

-- デックやパデックなどに含まれる円の曲線を、左端から反時計回りに生成します。
boneDot :: Given Config => Bone
boneDot = circle radius # rotateHalfTurn
  where
    radius = dotWidth / 2

-- デックやパデックなどに含まれる円を生成します。
-- 原点は円に外接する矩形の左下の角からオーバーシュート分だけ上に移動した位置にあります。
partDot :: Given Config => Part
partDot = partBy bones #.~> (0 &| originY)
  where
    bones = do
      boneDot
    originY = -dotWidth / 2 + overshoot

-- カルタックなどに含まれるベースラインより上に浮いた円を生成します。
-- partDot が返すパーツと形は同じですが、原点の位置が異なります。
-- 原点は左端にあります。
partFloatingDot :: Given Config => Part
partFloatingDot = partBy bones
  where
    bones = do
      boneDot

-- バデックの棒状の部分の直線を、上端から下端への向きで生成します。
boneBadekStem :: Given Config => Bone
boneBadekStem = origin ~~ (0 &| -height)
  where
    height = ascent - dotWidth - badekGap + overshoot

-- バデックの棒状の部分を生成します。
-- 原点は左下の角にあります。
partBadekStem :: Given Config => Part
partBadekStem = partBy bones
  where
    bones = do
      boneCut
      boneBadekStem # backward
      boneCut # backward
      boneBadekStem

-- パデックの棒状の部分の左側の曲線を、上端から下端への向きで生成します。
boneLeftPadekStem :: Given Config => Bone
boneLeftPadekStem = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = padekBend
    height = ascent - dotWidth - badekGap + overshoot
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = height * 0.55

-- パデックの棒状の部分の右側の曲線を、上端から下端への向きで生成します。
boneRightPadekStem :: Given Config => Bone
boneRightPadekStem = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = padekBend
    height = ascent - dotWidth - badekGap + overshoot
    topCont = height * 0.55
    bottomCont = searchTailInnerCont bend height topCont

-- パデックの棒状の部分を生成します。
-- 原点は左下の角にあります。
partPadekStem :: Given Config => Part
partPadekStem = partBy bones
  where
    bones = do
      boneCut
      boneRightPadekStem # backward
      boneCut # backward
      boneLeftPadekStem

-- ノークの棒状の部分の縦の曲線を、上端から下端への向きで生成します。
boneNokStem :: Given Config => Bone
boneNokStem = origin ~~ (0 &| -height)
  where
    height = nokHeight

-- ノークと同じ形を生成します。
-- 原点は左上の角にあります。
partNok :: Given Config => Part
partNok = partBy bones
  where
    bones = do
      boneNokStem
      boneCut
      boneNokStem # backward
      boneCut # backward

-- ディカックの棒状の部分の曲線を、上端から下端への向きで生成します。
boneDikakStem :: Given Config => Bone
boneDikakStem = origin ~> zero ~~ (0 &| leftCont) <~ (-bend &| -height)
  where
    bend = dikakBend
    height = dikakHeight
    leftCont = height * 0.6

-- ディカックと同じ形を生成します。
-- 原点は左上の角にあります。
partDikak :: Given Config => Part
partDikak = partBy bones
  where
    bones = do
      boneDikakStem
      boneCut
      boneDikakStem # backward
      boneCut # backward

-- フェークの直線を、左端から右端への向きで生成します。
boneFekHorizontal :: Given Config => Bone
boneFekHorizontal = origin ~~ (width &| 0)
  where
    width = fekWidth

-- フェークと同じ形を生成します。
-- 原点は左上の角にあります。
partFek :: Given Config => Part
partFek = partBy bones
  where
    bones = do
      boneVerticalCut
      boneFekHorizontal
      boneVerticalCut # backward
      boneFekHorizontal # backward

-- フォーハックの直線を、左端から右端への向きで生成します。
boneFohakHorizontal :: Given Config => Bone
boneFohakHorizontal = origin ~~ (width &| 0)
  where
    width = fohakWidth

-- フォーハックと同じ形を生成します。
-- 原点は左上の角にあります。
partFohak :: Given Config => Part
partFohak = partBy bones
  where
    bones = do
      boneVerticalCut
      boneFohakHorizontal
      boneVerticalCut # backward
      boneFohakHorizontal # backward

-- ダッシュの直線を、左端から右端への向きで生成します。
boneDashHorizontal :: Given Config => Bone
boneDashHorizontal = origin ~~ (width &| 0)
  where
    width = dashWidth

-- ダッシュと同じ形を生成します。
-- 原点は左上の角にあります。
partDash :: Given Config => Part
partDash = partBy bones
  where
    bones = do
      boneVerticalCut
      boneDashHorizontal
      boneVerticalCut # backward
      boneDashHorizontal # backward

-- ラクットの縦向きの棒状の部分の直線を、上端から下端への向きで生成します。
boneRakutVertical :: Given Config => Bone
boneRakutVertical = origin ~~ (0 &| -height)
  where
    height = rakutHeight

-- ラクットの横向きの棒状の部分の直線を、左端から右端への向きで生成します。
boneRakutHorizontal :: Given Config => Bone
boneRakutHorizontal = origin ~~ (width &| 0)
  where
    width = rakutWidth

-- ラクットの縦向きの棒状の部分を生成します。
-- 原点は左上の角にあります。
partRakutVertical :: Given Config => Part
partRakutVertical = partBy bones
  where
    bones = do
      boneRakutVertical
      boneCut
      boneRakutVertical # backward
      boneCut # backward

-- ラクットの横向きの棒状の部分を生成します。
-- 原点は左上の角にあります。
partRakutHorizontal :: Given Config => Part
partRakutHorizontal = partBy bones
  where
    bones = do
      boneVerticalCut
      boneRakutHorizontal
      boneVerticalCut # backward
      boneRakutHorizontal # backward

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