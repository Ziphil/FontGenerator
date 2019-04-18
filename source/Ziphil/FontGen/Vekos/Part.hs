{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos.Part
  ( partBowl
  , partTail
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
  , partTransphone
  , partAcute
  , partCircumflex
  , partDot
  , partBadekStem
  , partPadekStem
  , talWidth
  , narrowBowlWidth
  , xalWidth
  , nesWidth
  )
where

import Data.FontGen
import Data.Ord
import Data.List
import Data.Reflection
import Ziphil.FontGen.Vekos.Param


-- k, p, c, l, a などの文字に共通する丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterBowl :: Given Config => PartTrail
trailOuterBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height + overshoot)
  where
    width = bowlWidth / 2
    height = mean / 2

-- k, p, c, l, a などの文字に共通する丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerBowl :: Given Config => PartTrail
trailInnerBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height + overshoot)
  where
    width = bowlWidth / 2 - thicknessX
    height = mean / 2 - thicknessY

-- k, p, c, l, a などの文字に共通する丸い部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partBowl :: Given Config => Part
partBowl = mconcat parts # moveOriginBy (bowlWidth / 2 &| 0)
  where
    outerTrails =
      [ trailOuterBowl # reflectY
      , trailOuterBowl # rotateHalfTurn # reverseTrail
      , trailOuterBowl # reflectX
      , trailOuterBowl # reverseTrail
      ]
    innerTrails =
      [ trailInnerBowl # reflectY
      , trailInnerBowl # rotateHalfTurn # reverseTrail
      , trailInnerBowl # reflectX
      , trailInnerBowl # reverseTrail
      ]
    parts =
      [ makePart outerTrails
      , makePart innerTrails # reversePath # translate (thicknessX &| 0)
      ]

idealThickness :: Given Config => Angle Double -> Double
idealThickness angle =
  if angle <= quarterTurn
    then coeffX * thicknessX + coeffY * thicknessY
    else 1 / 0
  where
    coeffX = 1 - angleRatio angle quarterTurn
    coeffY = angleRatio angle quarterTurn

calcTailError :: Given Config => Double -> Double -> Double -> Double -> Double
calcTailError bend height innerCont outerCont = abs (distance point base - idealThickness angle / 2)
  where
    angle = angleBetween (point .-. base) (-1 &| 0)
    point = head $ closestPoint segment base
    base = (thicknessX / 2 - bend / 2 &| -height / 2)
    segment = origin ~> (0 &| -innerCont) ~:~ (0 &| outerCont) <~ (-bend &| -height)

searchTailInnerCont :: Given Config => Double -> Double -> Double -> Double
searchTailInnerCont bend height outerCont = minimumBy (comparing calcTailError') list
  where
    calcTailError' innerCont = calcTailError bend height innerCont outerCont
    list = [0, interval .. height]
    interval = 0.5

searchTailOuterCont :: Given Config => Double -> Double -> Double -> Double
searchTailOuterCont bend height innerCont = minimumBy (comparing calcTailError') list
  where
    calcTailError' outerCont = calcTailError bend height innerCont outerCont
    list = [0, interval .. height]
    interval = 0.5

-- l の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightTail :: Given Config => PartTrail
trailRightTail = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = tailBend
    height = mean / 2 + descent
    topCont = 270
    bottomCont = searchTailInnerCont bend height topCont

-- l の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftTail :: Given Config => PartTrail
trailLeftTail = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = tailBend
    height = mean / 2 + descent
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = 270

-- 文字の書き始めや書き終わりの位置にある水平に切られた部分を、左端から右端への向きで生成します。
trailCut :: Given Config => PartTrail
trailCut = origin ~~ (width &| 0)
  where
    width = thicknessX

-- l の文字のディセンダーを生成します。
-- 反転や回転を施すことで、c などの文字のディセンダーや k, p などの文字のアセンダーとしても使えます。
-- 原点は左上の角にあります。
partTail :: Given Config => Part
partTail = makePart trails
  where
    trails =
      [ trailLeftTail
      , trailCut
      , trailRightTail # reverseTrail
      , trailCut # reverseTrail
      ]

-- l の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partLes :: Given Config => Part
partLes = mconcat parts
  where
    parts =
      [ partBowl
      , partTail # translate (bowlWidth / 2 - thicknessX &| 0)
      ]

-- y の文字の下半分にある曲線を、上端から下端への向きで生成します。
trailLeg :: Given Config => PartTrail
trailLeg = origin ~> (0 &| -150) ~:~ zero <~ (bend &| -height)
  where
    bend = legBend
    height = mean / 2

-- y の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partYes :: Given Config => Part
partYes = makePart trails # moveOriginBy (bowlWidth / 2 &| 0)
  where
    trails =
      [ trailLeg
      , trailCut
      , trailLeg # reverseTrail
      , trailInnerBowl
      , trailInnerBowl # reflectX # reverseTrail
      , trailLeg # reflectX
      , trailCut
      , trailLeg # reflectX # reverseTrail
      , trailOuterBowl # reflectX
      , trailOuterBowl # reverseTrail
      ]

talWidth :: Given Config => Double
talWidth = bowlWidth / 2 + beakWidth

-- t の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
trailOuterBeak :: Given Config => PartTrail
trailOuterBeak = origin ~> (0 &| 10) ~:~ (width &| 0) <~ (-width &| height + overshoot)
  where
    width = beakWidth
    height = beakHeight

-- t の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
trailInnerBeak :: Given Config => PartTrail
trailInnerBeak = origin ~> (0 &| 10) ~:~ (width &| 0) <~ (-width &| height + overshoot)
  where
    width = beakWidth - thicknessX
    height = beakHeight - thicknessY

-- t の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTal :: Given Config => Part
partTal = makePart trails # moveOriginBy (talWidth / 2 &| 0)
  where
    trails =
      [ trailOuterBowl # reflectY
      , trailOuterBeak # reflectY # reverseTrail
      , trailCut # reverseTrail
      , trailInnerBeak # reflectY
      , trailInnerBowl # reflectY # reverseTrail
      , trailInnerBowl
      , trailInnerBeak # reverseTrail
      , trailCut
      , trailOuterBeak
      , trailOuterBowl # reverseTrail
      ]

narrowBowlWidth :: Given Config => Double
narrowBowlWidth = narrowBowlVirtualWidth - narrowBowlCorrection

xalWidth :: Given Config => Double
xalWidth = narrowBowlVirtualWidth * 2 - thicknessX

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterLeftNarrowBowl :: Given Config => PartTrail
trailOuterLeftNarrowBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、右端から上端への向きで生成します。
-- ただし、他のトレイルと使い方を揃えるため、左右反転してあります。
trailOuterRightNarrowBowl :: Given Config => PartTrail
trailOuterRightNarrowBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2

-- x, j の文字に共通する細い丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerNarrowBowl :: Given Config => PartTrail
trailInnerNarrowBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - thicknessX
    height = mean / 2 - thicknessY

-- x, j の文字に共通する細い丸い部分を生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は全体の中央にあります。
partNarrowBowl :: Given Config => Part
partNarrowBowl = mconcat parts # moveOriginBy (narrowBowlVirtualWidth / 2 &| 0)
  where
    outerTrails =
      [ trailOuterLeftNarrowBowl # reflectY
      , trailOuterRightNarrowBowl # rotateHalfTurn # reverseTrail
      , trailOuterRightNarrowBowl # reflectX
      , trailOuterLeftNarrowBowl # reverseTrail
      ]
    innerTrails =
      [ trailInnerNarrowBowl # reflectY
      , trailInnerNarrowBowl # rotateHalfTurn # reverseTrail
      , trailInnerNarrowBowl # reflectX
      , trailInnerNarrowBowl # reverseTrail
      ]
    parts =
      [ makePart outerTrails
      , makePart innerTrails # reversePath # translate (thicknessX &| 0)
      ]

-- x の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partXal :: Given Config => Part
partXal = mconcat parts # moveOriginBy (xalWidth / 2 - narrowBowlVirtualWidth / 2 &| 0)
  where
    parts =
      [ partNarrowBowl
      , partNarrowBowl # reflectX # reversePath # translate (narrowBowlVirtualWidth - thicknessX &| 0)
      ]

nesWidth :: Given Config => Double
nesWidth = narrowBowlVirtualWidth + spineWidth

-- n の文字の書き終わりの箇所にある曲線を、上端から下端への向きで生成します。
trailNesLeg :: Given Config => PartTrail
trailNesLeg = origin ~> (0 &| -150) ~:~ zero <~ (-bend &| -height)
  where
    bend = nesLegBend
    height = mean / 2

-- n の文字の中央部分の上側の曲線を、下端から上端への向きで生成します。
trailSpine :: Given Config => PartTrail
trailSpine = origin ~> (leftCont &| 0) ~:~ (-rightCont &| 0) <~ (width &| height + overshoot * 2)
  where
    width = spineWidth
    height = mean - thicknessY
    leftCont = width * 0.9
    rightCont = width * 0.4

-- n の文字と同じ形を生成します。
-- 原点は全体の中央にあります。
partNes :: Given Config => Part
partNes = makePart trails # moveOriginBy (nesWidth / 2 &| 0)
  where
    trails =
      [ trailOuterLeftNarrowBowl # reflectY
      , trailSpine
      , trailInnerNarrowBowl # reflectX # reverseTrail
      , trailNesLeg
      , trailCut
      , trailNesLeg # reverseTrail
      , trailOuterLeftNarrowBowl # reflectX
      , trailSpine # rotateHalfTurn
      , trailInnerNarrowBowl # reflectY # reverseTrail
      , trailNesLeg # rotateHalfTurn
      , trailCut # reverseTrail
      , trailNesLeg # rotateHalfTurn # reverseTrail
      ]

-- i の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftItTail :: Given Config => PartTrail
trailLeftItTail = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (bend &| -height)
  where
    bend = itTailBend
    height = mean / 2 + descent
    topCont = 300
    bottomCont = searchTailInnerCont bend height topCont

-- i の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightItTail :: Given Config => PartTrail
trailRightItTail = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (bend &| -height)
  where
    bend = itTailBend
    height = mean / 2 + descent
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = 300

-- i の文字と同じ形を生成します。
-- 原点は上部の丸い部分の中央にあるので、回転や反転で変化しません。
partIt :: Given Config => Part
partIt = makePart trails # moveOriginBy (talWidth / 2 &| 0)
  where
    trails =
      [ trailLeftItTail
      , trailCut
      , trailRightItTail # reverseTrail
      , trailInnerBowl
      , trailInnerBeak # reverseTrail
      , trailCut
      , trailOuterBeak
      , trailOuterBowl # reverseTrail
      ]

-- u の文字のディセンダーと接続する部分の外側の曲線を、上端から下端への向きで生成します。
trailOuterLink :: Given Config => PartTrail
trailOuterLink = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| -height)
  where
    width = linkWidth
    height = mean / 2 - linkLowerCorrection

-- u の文字のディセンダーと接続する部分の内側の曲線を、上端から下端への向きで生成します。
trailInnerLink :: Given Config => PartTrail
trailInnerLink = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| -height)
  where
    width = linkWidth - thicknessX
    height = mean / 2 - thicknessY

-- u の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftUtTail :: Given Config => PartTrail
trailLeftUtTail = origin ~> (0 &| 25) ~:~ (-bend &| 0) <~ (bend &| height)
  where
    bend = utTailBend
    height = descent + thicknessY - linkUpperCorrection

-- u の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightUtTail :: Given Config => PartTrail
trailRightUtTail = origin ~> (0 &| 25) ~:~ (-bend &| 0) <~ (bend &| height)
  where
    bend = utTailBend - thicknessX
    height = descent

-- u の文字のベースラインより上にある丸い部分を生成します。
-- ディセンダーと重ねたときに太く見えすぎないように、下側を少し細く補正してあります。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partUpperUt :: Given Config => Part
partUpperUt = makePart trails # moveOriginBy (talWidth / 2 &| 0)
  where
    trails =
      [ trailOuterLink
      , origin ~~ (0 &| thicknessY - linkLowerCorrection)
      , trailInnerLink # reverseTrail
      , trailInnerBowl
      , trailInnerBeak # reverseTrail
      , trailCut
      , trailOuterBeak
      , trailOuterBowl # reverseTrail
      ]

-- u の文字のディセンダーを生成します。
-- ベースラインより上の部分と重ねたときに太く見えすぎないように、上側を少し細く補正してあります。
-- 原点は右上の角にあります。
partUtTail :: Given Config => Part
partUtTail = makePart trails
  where
    trails =
      [ trailLeftUtTail # reverseTrail
      , trailCut
      , trailRightUtTail
      , origin ~~ (0 &| thicknessY - linkUpperCorrection)
      ]

-- u の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partUt :: Given Config => Part
partUt = mconcat parts
  where
    parts =
      [ partUpperUt
      , partUtTail # translate (-talWidth / 2 + linkWidth &| -mean / 2 + thicknessY - linkUpperCorrection)
      ]

-- 変音符の右に飛び出るように曲がる曲線の上半分を、上端から下端への向きで生成します。
trailTransphone :: Given Config => PartTrail
trailTransphone = origin ~> zero ~:~ (0 &| 150) <~ (bend &| -height)
  where
    bend = transphoneBend
    height = mean / 2

-- 変音符の上下にある水平に切られた部分を、左端から右端への向きで生成します。
trailTransphoneCut :: Given Config => PartTrail
trailTransphoneCut = origin ~~ (width &| 0)
  where
    width = transphoneThicknessX

-- 変音符と同じ形を生成します。
-- 原点は左下の角にあります。
partTransphone :: Given Config => Part
partTransphone = makePart trails # moveOriginBy (0 &| -mean)
  where
    trails = 
      [ trailTransphone
      , trailTransphone # reflectY # reverseTrail
      , trailTransphoneCut
      , trailTransphone # reflectY
      , trailTransphone # reverseTrail
      , trailTransphoneCut # reverseTrail
      ]
  
-- アキュートアクセントの丸い部分の外側の曲線の半分を、左下端から上端への向きで生成します。
trailOuterAcute :: Given Config => PartTrail
trailOuterAcute = origin ~> (0 &| 10) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = acuteWidth / 2
    height = acuteHeight
    
-- アキュートアクセントの丸い部分の内側の曲線の半分を、左下端から上端への向きで生成します。
trailInnerAcute :: Given Config => PartTrail
trailInnerAcute = origin ~> (0 &| 10) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = acuteWidth / 2 - acuteThicknessX
    height = acuteHeight - acuteThicknessY

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
      , trailInnerAcute # reflectX # reverseTrail
      , trailAcuteCut
      , trailOuterAcute # reflectX
      , trailOuterAcute # reverseTrail
      ]

-- サーカムフレックスアクセントの外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterCircumflex :: Given Config => PartTrail
trailOuterCircumflex = origin ~> (0 &| 10) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = circumflexWidth / 2
    height = circumflexHeight / 2

-- サーカムフレックスアクセントの内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerCircumflex :: Given Config => PartTrail
trailInnerCircumflex = origin ~> (0 &| 10) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = circumflexWidth / 2 - circumflexThicknessX
    height = circumflexHeight / 2 - circumflexThicknessY

-- サーカムフレックスアクセントと同じ形を生成します。
-- 原点は下部中央にあります。
partCircumflex :: Given Config => Part
partCircumflex = mconcat parts # moveOriginBy (circumflexWidth / 2 &| -circumflexHeight / 2)
  where
    outerTrails =
      [ trailOuterCircumflex # reflectY
      , trailOuterCircumflex # rotateHalfTurn # reverseTrail
      , trailOuterCircumflex # reflectX
      , trailOuterCircumflex # reverseTrail
      ]
    innerTrails =
      [ trailInnerCircumflex # reflectY
      , trailInnerCircumflex # rotateHalfTurn # reverseTrail
      , trailInnerCircumflex # reflectX
      , trailInnerCircumflex # reverseTrail
      ]
    parts =
      [ makePart outerTrails
      , makePart innerTrails # reversePath # translate (circumflexThicknessX &| 0)
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

-- ! の文字の棒状の部分の直線を、上端から下端への向きで生成します。
trailBadekStem :: Given Config => PartTrail
trailBadekStem = origin ~~ (0 &| -height)
  where
    height = ascent - dotWidth - badekGap + overshoot

-- ! の文字の棒状の部分を生成します。
-- 原点は左下の角にあります。
partBadekStem :: Given Config => Part
partBadekStem = makePart trails
  where
    trails =
      [ trailCut
      , trailBadekStem # reverseTrail
      , trailCut # reverseTrail
      , trailBadekStem
      ]

-- ? の文字の棒状の部分の左側の曲線を、上端から下端への向きで生成します。
trailLeftPadekStem :: Given Config => PartTrail
trailLeftPadekStem = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = dotWidth + dotGap
    height = ascent - dotWidth - badekGap + overshoot
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = 300

-- ? の文字の棒状の部分の右側の曲線を、上端から下端への向きで生成します。
trailRightPadekStem :: Given Config => PartTrail
trailRightPadekStem = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = dotWidth + dotGap
    height = ascent - dotWidth - badekGap + overshoot
    topCont = 300
    bottomCont = searchTailInnerCont bend height topCont

-- ? の文字の棒状の部分を生成します。
-- 原点は左下の角にあります。
partPadekStem :: Given Config => Part
partPadekStem = makePart trails
  where
    trails =
      [ trailCut
      , trailRightPadekStem # reverseTrail
      , trailCut # reverseTrail
      , trailLeftPadekStem
      ]