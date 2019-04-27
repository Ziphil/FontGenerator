{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}


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
  , partTasFrame
  , partCrossbar
  , partTas
  , partTransphone
  , partAcute
  , partCircumflex
  , partDot
  , partBadekStem
  , partPadekStem
  , partNok
  , partDikak
  , partFek
  , partOpeningRakut
  , talWidth
  , narrowBowlWidth
  , xalWidth
  , nesWidth
  , tasWidth
  )
where

import Data.FontGen
import Data.Ord
import Data.List
import Ziphil.FontGen.Vekos.Config
import Ziphil.FontGen.Vekos.Util
import Ziphil.FontGen.Vekos.Value


-- k, p, c, l, a などの文字に共通する丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterBowl :: Given Config => PartTrail
trailOuterBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = bowlWidth / 2
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- k, p, c, l, a などの文字に共通する丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerBowl :: Given Config => PartTrail
trailInnerBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = bowlWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot
    leftCont = height * 0.1
    topCont = width

-- k, p, c, l, a などの文字に共通する丸い部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partBowl :: Given Config => Part
partBowl = concatPath paths # moveOriginBy (bowlWidth / 2 &| 0)
  where
    outerTrails =
      [ trailOuterBowl # reflectY
      , trailOuterBowl # rotateHalfTurn # backward
      , trailOuterBowl # reflectX
      , trailOuterBowl # backward
      ]
    innerTrails =
      [ trailInnerBowl # reflectY
      , trailInnerBowl # rotateHalfTurn # backward
      , trailInnerBowl # reflectX
      , trailInnerBowl # backward
      ]
    paths =
      [ makePath outerTrails
      , makePath innerTrails # backward # translate (thicknessX &| 0)
      ]

idealThickness :: Given Config => Angle Double -> Double
idealThickness angle =
  if angle >= zero && angle <= quarterTurn
    then coeffX * thicknessX + coeffY * thicknessY
    else 1 / 0
  where
    coeffX = angleRatio angle quarterTurn
    coeffY = 1 - angleRatio angle quarterTurn

calcTailError :: Given Config => Double -> Double -> Double -> Double -> Double
calcTailError bend height innerCont outerCont = abs (distance point base - idealThickness angle / 2)
  where
    angle = angleBetween (point .-. base) unitX ^-^ quarterTurn
    point = head $ closestPoint segment base
    base = (-bend / 2 + thicknessX / 2 &| -height / 2)
    segment = origin ~> (0 &| -innerCont) ~~ (0 &| outerCont) <~ (-bend &| -height)

searchTailInnerCont :: Given Config => Double -> Double -> Double -> Double
searchTailInnerCont bend height outerCont = minimumBy (comparing calcTailError') list
  where
    calcTailError' innerCont = calcTailError bend height innerCont outerCont
    list = [0, interval .. height]
    interval = 0.5

-- l の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftLesTail :: Given Config => PartTrail
trailLeftLesTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = lesTailBend - thicknessX / 2 + lesTailCorrection
    virtualBend = lesTailBend
    height = mean / 2 + descent
    topCont = searchTailInnerCont virtualBend height bottomCont
    bottomCont = descent * 1.08

-- l の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightLesTail :: Given Config => PartTrail
trailRightLesTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = lesTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = descent * 1.08
    bottomCont = searchTailInnerCont bend height topCont

-- 文字の書き始めや書き終わりの位置にある水平に切られた部分を、左端から右端への向きで生成します。
trailCut :: Given Config => PartTrail
trailCut = origin ~~ (width &| 0)
  where
    width = thicknessX

-- l の文字のディセンダーを生成します。
-- 反転や回転を施すことで、c などの文字のディセンダーや k, p などの文字のアセンダーとしても使えます。
-- 丸い部分と重ねたときに重なった部分が太く見えすぎないように、左側を少し細く補正してあります。
-- 原点は補正がないとしたときの左上の角にあります。
partLesTail :: Given Config => Part
partLesTail = makePart trails # moveOriginBy (-lesTailCorrection &| 0)
  where
    trails =
      [ trailLeftLesTail
      , trailCut
      , trailRightLesTail # backward
      , origin ~~ (-thicknessX + lesTailCorrection &| 0)
      ]

-- l の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partLes :: Given Config => Part
partLes = concat parts
  where
    parts =
      [ partBowl
      , partLesTail # translate (bowlWidth / 2 - thicknessX &| 0)
      ]

-- y の文字の下半分にある曲線を、上端から下端への向きで生成します。
trailYesLeg :: Given Config => PartTrail
trailYesLeg = origin ~> (0 &| -leftCont) ~~ zero <~ (bend &| -height)
  where
    bend = yesLegBend
    height = mean / 2
    leftCont = height * 0.6

-- y の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partYes :: Given Config => Part
partYes = makePart trails # moveOriginBy (bowlWidth / 2 &| 0)
  where
    trails =
      [ trailYesLeg
      , trailCut
      , trailYesLeg # backward
      , trailInnerBowl
      , trailInnerBowl # reflectX # backward
      , trailYesLeg # reflectX
      , trailCut
      , trailYesLeg # reflectX # backward
      , trailOuterBowl # reflectX
      , trailOuterBowl # backward
      ]

talWidth :: Given Config => Double
talWidth = bowlWidth / 2 + talBeakWidth

-- t の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
trailOuterTalBeak :: Given Config => PartTrail
trailOuterTalBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = talBeakWidth
    height = talBeakHeight + overshoot
    rightCont = height * 0.05
    topCont = width

-- t の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
trailInnerTalBeak :: Given Config => PartTrail
trailInnerTalBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = talBeakWidth - thicknessX
    height = talBeakHeight - thicknessY + overshoot
    rightCont = height * 0.05
    topCont = width

-- t の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTal :: Given Config => Part
partTal = makePart trails # moveOriginBy (talWidth / 2 &| 0)
  where
    trails =
      [ trailOuterBowl # reflectY
      , trailOuterTalBeak # reflectY # backward
      , trailCut # backward
      , trailInnerTalBeak # reflectY
      , trailInnerBowl # reflectY # backward
      , trailInnerBowl
      , trailInnerTalBeak # backward
      , trailCut
      , trailOuterTalBeak
      , trailOuterBowl # backward
      ]

narrowBowlWidth :: Given Config => Double
narrowBowlWidth = narrowBowlVirtualWidth - narrowBowlCorrection

xalWidth :: Given Config => Double
xalWidth = narrowBowlVirtualWidth * 2 - thicknessX

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterLeftNarrowBowl :: Given Config => PartTrail
trailOuterLeftNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、右端から上端への向きで生成します。
-- ただし、他のトレイルと使い方を揃えるため、左右反転してあります。
trailOuterRightNarrowBowl :: Given Config => PartTrail
trailOuterRightNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2 + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerNarrowBowl :: Given Config => PartTrail
trailInnerNarrowBowl = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot
    leftCont = height * 0.1
    topCont = width

-- x, j の文字に共通する細い丸い部分を生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は全体の中央にあります。
partNarrowBowl :: Given Config => Part
partNarrowBowl = concatPath paths # moveOriginBy (narrowBowlVirtualWidth / 2 &| 0)
  where
    outerTrails =
      [ trailOuterLeftNarrowBowl # reflectY
      , trailOuterRightNarrowBowl # rotateHalfTurn # backward
      , trailOuterRightNarrowBowl # reflectX
      , trailOuterLeftNarrowBowl # backward
      ]
    innerTrails =
      [ trailInnerNarrowBowl # reflectY
      , trailInnerNarrowBowl # rotateHalfTurn # backward
      , trailInnerNarrowBowl # reflectX
      , trailInnerNarrowBowl # backward
      ]
    paths =
      [ makePath outerTrails
      , makePath innerTrails # backward # translate (thicknessX &| 0)
      ]

-- x の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partXal :: Given Config => Part
partXal = concat parts # moveOriginBy (xalWidth / 2 - narrowBowlVirtualWidth / 2 &| 0)
  where
    parts =
      [ partNarrowBowl
      , partNarrowBowl # reflectX # translate (narrowBowlVirtualWidth - thicknessX &| 0)
      ]

nesWidth :: Given Config => Double
nesWidth = narrowBowlVirtualWidth + spineWidth

calcSpineError :: Given Config => Double -> Double -> Double -> Double -> Double
calcSpineError bend width innerCont outerCont = abs (distance point base - idealThickness angle / 2)
  where
    angle = angleBetween (point .-. base) unitX ^-^ quarterTurn
    point = head $ closestPoint segment base
    base = (width / 2 &| bend / 2 - thicknessY / 2)
    segment = origin ~> (innerCont &| 0) ~~ (-outerCont &| 0) <~ (width &| bend)

searchSpineInnerCont :: Given Config => Double -> Double -> Double -> Double
searchSpineInnerCont bend width outerCont = minimumBy (comparing calcSpineError') list
  where
    calcSpineError' innerCont = calcSpineError bend width innerCont outerCont
    list = [0, interval .. width]
    interval = 0.5

-- n の文字の書き終わりの箇所にある曲線を、上端から下端への向きで生成します。
trailNesLeg :: Given Config => PartTrail
trailNesLeg = origin ~> (0 &| -rightCont) ~~ zero <~ (-bend &| -height)
  where
    bend = nesLegBend
    height = mean / 2
    rightCont = height * 0.6

-- n の文字の中央部分の上側の曲線を、下端から上端への向きで生成します。
trailTopSpine :: Given Config => PartTrail
trailTopSpine = origin ~> (leftCont &| 0) ~~ (-rightCont &| 0) <~ (width &| bend)
  where
    width = spineWidth
    bend = mean - thicknessY + overshoot * 2
    leftCont = searchSpineInnerCont bend width rightCont
    rightCont = width * 1.05

-- n の文字の中央部分の下側の曲線を、下端から上端への向きで生成します。
trailBottomSpine :: Given Config => PartTrail
trailBottomSpine = origin ~> (leftCont &| 0) ~~ (-rightCont &| 0) <~ (width &| bend)
  where
    width = spineWidth
    bend = mean - thicknessY + overshoot * 2
    leftCont = width * 1.05
    rightCont = searchSpineInnerCont bend width leftCont

-- n の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partNes :: Given Config => Part
partNes = makePart trails # moveOriginBy (nesWidth / 2 &| 0)
  where
    trails =
      [ trailOuterLeftNarrowBowl # reflectY
      , trailBottomSpine
      , trailInnerNarrowBowl # reflectX # backward
      , trailNesLeg
      , trailCut
      , trailNesLeg # backward
      , trailOuterLeftNarrowBowl # reflectX
      , trailTopSpine # backward
      , trailInnerNarrowBowl # reflectY # backward
      , trailNesLeg # rotateHalfTurn
      , trailCut # backward
      , trailNesLeg # rotateHalfTurn # backward
      ]

-- i の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftItTail :: Given Config => PartTrail
trailLeftItTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (bend &| -height)
  where
    bend = itTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = descent * 1.2
    bottomCont = searchTailInnerCont bend height topCont

-- i の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightItTail :: Given Config => PartTrail
trailRightItTail = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (bend &| -height)
  where
    bend = itTailBend - thicknessX / 2
    height = mean / 2 + descent
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = descent * 1.2

-- i の文字と同じ形を生成します。
-- 原点は上部の丸い部分の中央にあるので、回転や反転で変化しません。
partIt :: Given Config => Part
partIt = makePart trails # moveOriginBy (talWidth / 2 &| 0)
  where
    trails =
      [ trailLeftItTail
      , trailCut
      , trailRightItTail # backward
      , trailInnerBowl
      , trailInnerTalBeak # backward
      , trailCut
      , trailOuterTalBeak
      , trailOuterBowl # backward
      ]

-- u の文字のディセンダーと接続する部分の外側の曲線を、上端から下端への向きで生成します。
trailOuterLink :: Given Config => PartTrail
trailOuterLink = origin ~> (0 &| -leftCont) ~~ (-bottomCont &| 0) <~ (width &| -height)
  where
    width = linkWidth
    height = mean / 2 - linkLowerCorrection
    leftCont = height * 0.02
    bottomCont = width

-- u の文字のディセンダーと接続する部分の内側の曲線を、上端から下端への向きで生成します。
trailInnerLink :: Given Config => PartTrail
trailInnerLink = origin ~> (0 &| -leftCont) ~~ (-bottomCont &| 0) <~ (width &| -height)
  where
    width = linkWidth - thicknessX
    height = mean / 2 - thicknessY
    leftCont = height * 0.02
    bottomCont = width

-- u の文字のディセンダーの左側の曲線を、下端から上端への向きで生成します。
trailLeftUtTail :: Given Config => PartTrail
trailLeftUtTail = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (bend &| height)
  where
    bend = utTailBend + thicknessX / 2
    height = descent + thicknessY - linkUpperCorrection
    leftCont = height * 0.1
    topCont = bend

-- u の文字のディセンダーの右側の曲線を、下端から上端への向きで生成します。
trailRightUtTail :: Given Config => PartTrail
trailRightUtTail = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (bend &| height)
  where
    bend = utTailBend - thicknessX / 2
    height = descent
    leftCont = height * 0.1
    topCont = bend

-- u の文字のベースラインより上にある丸い部分を生成します。
-- ディセンダーと重ねたときに太く見えすぎないように、下側を少し細く補正してあります。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partUpperUt :: Given Config => Part
partUpperUt = makePart trails # moveOriginBy (talWidth / 2 &| 0)
  where
    trails =
      [ trailOuterLink
      , origin ~~ (0 &| thicknessY - linkLowerCorrection)
      , trailInnerLink # backward
      , trailInnerBowl
      , trailInnerTalBeak # backward
      , trailCut
      , trailOuterTalBeak
      , trailOuterBowl # backward
      ]

-- u の文字のディセンダーを生成します。
-- ベースラインより上の部分と重ねたときに太く見えすぎないように、上側を少し細く補正してあります。
-- 原点は右上の角にあります。
partUtTail :: Given Config => Part
partUtTail = makePart trails
  where
    trails =
      [ trailLeftUtTail # backward
      , trailCut
      , trailRightUtTail
      , origin ~~ (0 &| thicknessY - linkUpperCorrection)
      ]

-- u の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partUt :: Given Config => Part
partUt = concat parts
  where
    parts =
      [ partUpperUt
      , partUtTail # translate (-talWidth / 2 + linkWidth &| -mean / 2 + thicknessY - linkUpperCorrection)
      ]

-- 6 の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partRac :: Given Config => Part
partRac = concat parts
  where
    parts =
      [ partYes # rotateHalfTurn
      , partLesTail # translate (bowlWidth / 2 - thicknessX &| 0)
      ]

solidusLength :: Given Config => Double
solidusLength = rawLength * 2 - thicknessX
  where
    rawLength = distance origin $ head $ intersectPointsP line (head partBowl)
    line = origin ~~ (bowlWidth / 2 &| solidusGrade)

solidusThickness :: Given Config => Double
solidusThickness = idealThickness solidusAngle

solidusAngle :: Given Config => Angle Double
solidusAngle = signedAngleBetween (bowlWidth / 2 &| solidusGrade) unitX

-- 0 の文字の斜線の部分の長い方の直線を、左端から右端への向きで生成します。
-- パーツを構成した後に回転することを想定しているので、このトレイルは水平です。
trailSolidus :: Given Config => PartTrail
trailSolidus = origin ~~ (length &| 0)
  where
    length = solidusLength

-- 0 の文字の斜線の部分の短い方の直線を、上端から下端への向きで生成します。
-- パーツを構成した後に回転することを想定しているので、このトレイルは鉛直です。
trailSolidusCut :: Given Config => PartTrail
trailSolidusCut = origin ~~ (0 &| -length)
  where
    length = solidusThickness

-- 0 の文字の斜線の部分を生成します。
-- 原点は全体の中央にあります。
partSolidus :: Given Config => Part
partSolidus = makePart trails # moveOriginBy (solidusLength / 2 &| -solidusThickness / 2) # rotate solidusAngle
  where
    trails =
      [ trailSolidusCut
      , trailSolidus
      , trailSolidusCut # backward
      , trailSolidus # backward
      ]

-- 0 の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partNuf :: Given Config => Part
partNuf = concat parts
  where
    parts =
      [ partBowl
      , partSolidus
      ]

tasWidth :: Given Config => Double
tasWidth = bowlWidth / 2 + max shoulderWidth tasBeakWidth

-- 1 の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
trailOuterTasBeak :: Given Config => PartTrail
trailOuterTasBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = tasBeakWidth
    height = tasBeakHeight + overshoot
    rightCont = height * 0.05
    topCont = width

-- 1 の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
trailInnerTasBeak :: Given Config => PartTrail
trailInnerTasBeak = origin ~> (0 &| rightCont) ~~ (topCont &| 0) <~ (-width &| height)
  where
    width = tasBeakWidth - thicknessX
    height = tasBeakHeight - thicknessY + overshoot
    rightCont = height * 0.05
    topCont = width

-- 1 の文字の右下にある部分の外側の曲線を、上端から下端への向きで生成します。
trailOuterShoulder :: Given Config => PartTrail
trailOuterShoulder = origin ~> (0 &| -rightCont) ~~ (bottomCont &| 0) <~ (-width &| -height)
  where
    width = shoulderWidth
    height = crossbarAltitude + thicknessY / 2 - shoulderStraightHeight + overshoot
    rightCont = height * 0.1
    bottomCont = width

-- 1 の文字の右下にある部分の内側の曲線を、上端から下端への向きで生成します。
trailInnerShoulder :: Given Config => PartTrail
trailInnerShoulder = origin ~> (0 &| -rightCont) ~~ (bottomCont &| 0) <~ (-width &| -height)
  where
    width = shoulderWidth - thicknessX
    height = crossbarAltitude - thicknessY / 2 - shoulderStraightHeight + overshoot
    rightCont = height * 0.1
    bottomCont = width

-- 1 の文字の右下にある部分に含まれる直線を、上端から下端への向きで生成します。
trailShoulderStraight :: Given Config => PartTrail
trailShoulderStraight = origin ~~ (0 &| height)
  where
    height = shoulderStraightHeight

-- 1 の文字の横線以外の部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTasFrame :: Given Config => Part
partTasFrame = makePart trails # moveOriginBy (tasWidth / 2 &| 0)
  where
    trails =
      [ trailOuterBowl # reflectY
      , trailOuterShoulder # backward
      , trailShoulderStraight
      , trailCut # backward
      , trailShoulderStraight # backward
      , trailInnerShoulder
      , trailInnerBowl # reflectY # backward
      , trailInnerBowl
      , trailInnerTasBeak # backward
      , trailCut
      , trailOuterTasBeak
      , trailOuterBowl # backward
      ]

-- 1 の文字の横線の部分の直線を、左端から右端への向きで生成します。
trailCrossbar :: Given Config => PartTrail
trailCrossbar = origin ~~ (width &| 0)
  where
    width = bowlWidth / 2 + shoulderWidth - thicknessX

-- 文字の書き始めや書き終わりの位置にある垂直に切られた部分を、上端から下端への向きで生成します。
trailVerticalCut :: Given Config => PartTrail
trailVerticalCut = origin ~~ (0 &| -height)
  where
    height = thicknessY

-- 1 の文字と同じ形を生成します。
-- 原点は左上の角にあります。
partCrossbar :: Given Config => Part
partCrossbar = makePart trails
  where
    trails =
      [ trailVerticalCut
      , trailCrossbar
      , trailVerticalCut # backward
      , trailCrossbar # backward
      ]

-- 1 の文字の横線以外の部分を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partTas :: Given Config => Part
partTas = concat parts
  where
    parts =
      [ partTasFrame
      , partCrossbar # translate (thicknessX / 2 - tasWidth / 2 &| crossbarAltitude - mean / 2 + thicknessY / 2)
      ]

-- 変音符の右に飛び出るように曲がる曲線の上半分を、下端から上端への向きで生成します。
trailTransphone :: Given Config => PartTrail
trailTransphone = origin ~> zero ~~ (0 &| rightCont) <~ (bend &| -height)
  where
    bend = transphoneBend
    height = mean / 2
    rightCont = height * 0.6

-- 変音符の上下にある水平に切られた部分を、左端から右端への向きで生成します。
trailTransphoneCut :: Given Config => PartTrail
trailTransphoneCut = origin ~~ (width &| 0)
  where
    width = transphoneThicknessX

-- 変音符と同じ形を生成します。
-- 原点は右に飛び出る部分の左中央にあります。
partTransphone :: Given Config => Part
partTransphone = makePart trails # moveOriginBy (transphoneBend &| -mean / 2)
  where
    trails = 
      [ trailTransphone
      , trailTransphone # reflectY # backward
      , trailTransphoneCut
      , trailTransphone # reflectY
      , trailTransphone # backward
      , trailTransphoneCut # backward
      ]
  
-- アキュートアクセントの丸い部分の外側の曲線の半分を、左下端から上端への向きで生成します。
trailOuterAcute :: Given Config => PartTrail
trailOuterAcute = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = acuteWidth / 2
    height = acuteHeight
    leftCont = height * 0.1
    topCont = width
    
-- アキュートアクセントの丸い部分の内側の曲線の半分を、左下端から上端への向きで生成します。
trailInnerAcute :: Given Config => PartTrail
trailInnerAcute = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = acuteWidth / 2 - acuteThicknessX
    height = acuteHeight - acuteThicknessY
    leftCont = height * 0.1
    topCont = width

-- アキュートアクセントの下部にある水平に切られた部分を、左端から右端への向きで生成します。
trailAcuteCut :: Given Config => PartTrail
trailAcuteCut = origin ~~ (width &| 0)
  where
    width = acuteThicknessX

-- アキュートアクセントと同じ形を生成します。
-- 原点は下部中央にあります。
partAcute :: Given Config => Part
partAcute = makePart trails # moveOriginBy (acuteWidth / 2 &| 0)
  where
    trails =
      [ trailAcuteCut
      , trailInnerAcute
      , trailInnerAcute # reflectX # backward
      , trailAcuteCut
      , trailOuterAcute # reflectX
      , trailOuterAcute # backward
      ]

-- サーカムフレックスアクセントの外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterCircumflex :: Given Config => PartTrail
trailOuterCircumflex = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = circumflexWidth / 2
    height = circumflexHeight / 2
    leftCont = height * 0.1
    topCont = width

-- サーカムフレックスアクセントの内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerCircumflex :: Given Config => PartTrail
trailInnerCircumflex = origin ~> (0 &| leftCont) ~~ (-topCont &| 0) <~ (width &| height)
  where
    width = circumflexWidth / 2 - circumflexThicknessX
    height = circumflexHeight / 2 - circumflexThicknessY
    leftCont = height * 0.1
    topCont = width

-- サーカムフレックスアクセントと同じ形を生成します。
-- 原点は下部中央にあります。
partCircumflex :: Given Config => Part
partCircumflex = concatPath paths # moveOriginBy (circumflexWidth / 2 &| -circumflexHeight / 2)
  where
    outerTrails =
      [ trailOuterCircumflex # reflectY
      , trailOuterCircumflex # rotateHalfTurn # backward
      , trailOuterCircumflex # reflectX
      , trailOuterCircumflex # backward
      ]
    innerTrails =
      [ trailInnerCircumflex # reflectY
      , trailInnerCircumflex # rotateHalfTurn # backward
      , trailInnerCircumflex # reflectX
      , trailInnerCircumflex # backward
      ]
    paths =
      [ makePath outerTrails
      , makePath innerTrails # backward # translate (circumflexThicknessX &| 0)
      ]

-- デックやパデックなどに含まれる円の曲線を、左端から時計回りに生成します。
trailDot :: Given Config => PartTrail
trailDot = circle radius # rotateHalfTurn
  where
    radius = dotWidth / 2

-- デックやパデックなどに含まれる円を生成します。
-- 原点は円に外接する矩形の左下の角からオーバーシュート分だけ上に移動した位置にあります。
partDot :: Given Config => Part
partDot = makePart trails # moveOriginBy (0 &| -dotWidth / 2 + overshoot)
  where
    trails =
      [ trailDot
      ]

-- バデックの棒状の部分の直線を、上端から下端への向きで生成します。
trailBadekStem :: Given Config => PartTrail
trailBadekStem = origin ~~ (0 &| -height)
  where
    height = ascent - dotWidth - badekGap + overshoot

-- バデックの棒状の部分を生成します。
-- 原点は左下の角にあります。
partBadekStem :: Given Config => Part
partBadekStem = makePart trails
  where
    trails =
      [ trailCut
      , trailBadekStem # backward
      , trailCut # backward
      , trailBadekStem
      ]

-- パデックの棒状の部分の左側の曲線を、上端から下端への向きで生成します。
trailLeftPadekStem :: Given Config => PartTrail
trailLeftPadekStem = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = padekBend
    height = ascent - dotWidth - badekGap + overshoot
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = height * 0.55

-- パデックの棒状の部分の右側の曲線を、上端から下端への向きで生成します。
trailRightPadekStem :: Given Config => PartTrail
trailRightPadekStem = origin ~> (0 &| -topCont) ~~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = padekBend
    height = ascent - dotWidth - badekGap + overshoot
    topCont = height * 0.55
    bottomCont = searchTailInnerCont bend height topCont

-- パデックの棒状の部分を生成します。
-- 原点は左下の角にあります。
partPadekStem :: Given Config => Part
partPadekStem = makePart trails
  where
    trails =
      [ trailCut
      , trailRightPadekStem # backward
      , trailCut # backward
      , trailLeftPadekStem
      ]

-- ノークの棒状の部分の縦の曲線を、上端から下端への向きで生成します。
trailNokStem :: Given Config => PartTrail
trailNokStem = origin ~~ (0 &| -height)
  where
    height = nokHeight

-- ノークと同じ形を生成します。
-- 原点は左上の角にあります。
partNok :: Given Config => Part
partNok = makePart trails
  where
    trails =
      [ trailNokStem
      , trailCut
      , trailNokStem # backward
      , trailCut # backward
      ]

-- ディカックの棒状の部分の曲線を、上端から下端への向きで生成します。
trailDikakStem :: Given Config => PartTrail
trailDikakStem = origin ~> zero ~~ (0 &| leftCont) <~ (-bend &| -height)
  where
    bend = dikakBend
    height = dikakHeight
    leftCont = height * 0.6

-- ディカックと同じ形を生成します。
-- 原点は左上の角にあります。
partDikak :: Given Config => Part
partDikak = makePart trails
  where
    trails =
      [ trailDikakStem
      , trailCut
      , trailDikakStem # backward
      , trailCut # backward
      ]

-- フェークの直線を、左端から右端への向きで生成します。
trailFekHorizontal :: Given Config => PartTrail
trailFekHorizontal = origin ~~ (width &| 0)
  where
    width = fekWidth

partFek :: Given Config => Part
partFek = makePart trails
  where
    trails =
      [ trailVerticalCut
      , trailFekHorizontal
      , trailVerticalCut # backward
      , trailFekHorizontal # backward
      ]

-- ラクットの縦向きの棒状の部分の直線を、上端から下端への向きで生成します。
trailRakutVertical :: Given Config => PartTrail
trailRakutVertical = origin ~~ (0 &| -height)
  where
    height = rakutHeight

-- ラクットの横向きの棒状の部分の直線を、左端から右端への向きで生成します。
trailRakutHorizontal :: Given Config => PartTrail
trailRakutHorizontal = origin ~~ (width &| 0)
  where
    width = rakutWidth

-- ラクットの縦向きの棒状の部分を生成します。
-- 原点は左上の角にあります。
partRakutVertical :: Given Config => Part
partRakutVertical = makePart trails
  where
    trails =
      [ trailRakutVertical
      , trailCut
      , trailRakutVertical # backward
      , trailCut # backward
      ]

-- ラクットの横向きの棒状の部分を生成します。
-- 原点は左上の角にあります。
partRakutHorizontal :: Given Config => Part
partRakutHorizontal = makePart trails
  where
    trails =
      [ trailVerticalCut
      , trailRakutHorizontal
      , trailVerticalCut # backward
      , trailRakutHorizontal # backward
      ]

-- 開きラクットと同じ形を生成します。
-- 原点は左上の角にあります。
partOpeningRakut :: Given Config => Part
partOpeningRakut = concat parts
  where
    parts =
      [ partRakutVertical
      , partRakutHorizontal
      ]