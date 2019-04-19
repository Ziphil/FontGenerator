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
  , partNok
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
trailOuterBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = bowlWidth / 2
    height = mean / 2 + overshoot

-- k, p, c, l, a などの文字に共通する丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerBowl :: Given Config => PartTrail
trailInnerBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = bowlWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot

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
    angle = angleBetween (point .-. base) (1 &| 0) ^-^ quarterTurn
    point = head $ closestPoint segment base
    base = (-bend / 2 + thicknessX / 2 &| -height / 2)
    segment = origin ~> (0 &| -innerCont) ~:~ (0 &| outerCont) <~ (-bend &| -height)

searchTailInnerCont :: Given Config => Double -> Double -> Double -> Double
searchTailInnerCont bend height outerCont = minimumBy (comparing calcTailError') list
  where
    calcTailError' innerCont = calcTailError bend height innerCont outerCont
    list = [0, interval .. height]
    interval = 0.5

-- l の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftLesTail :: Given Config => PartTrail
trailLeftLesTail = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = lesTailBend
    height = mean / 2 + descent
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = 270

-- l の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightLesTail :: Given Config => PartTrail
trailRightLesTail = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = lesTailBend
    height = mean / 2 + descent
    topCont = 270
    bottomCont = searchTailInnerCont bend height topCont

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
      [ trailLeftLesTail
      , trailCut
      , trailRightLesTail # backward
      , trailCut # backward
      ]

-- l の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partLes :: Given Config => Part
partLes = concat parts
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
      , trailLeg # backward
      , trailInnerBowl
      , trailInnerBowl # reflectX # backward
      , trailLeg # reflectX
      , trailCut
      , trailLeg # reflectX # backward
      , trailOuterBowl # reflectX
      , trailOuterBowl # backward
      ]

talWidth :: Given Config => Double
talWidth = bowlWidth / 2 + beakWidth

-- t の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
trailOuterBeak :: Given Config => PartTrail
trailOuterBeak = origin ~> (0 &| 10) ~:~ (width &| 0) <~ (-width &| height)
  where
    width = beakWidth
    height = beakHeight + overshoot

-- t の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
trailInnerBeak :: Given Config => PartTrail
trailInnerBeak = origin ~> (0 &| 10) ~:~ (width &| 0) <~ (-width &| height)
  where
    width = beakWidth - thicknessX
    height = beakHeight - thicknessY + overshoot

-- t の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTal :: Given Config => Part
partTal = makePart trails # moveOriginBy (talWidth / 2 &| 0)
  where
    trails =
      [ trailOuterBowl # reflectY
      , trailOuterBeak # reflectY # backward
      , trailCut # backward
      , trailInnerBeak # reflectY
      , trailInnerBowl # reflectY # backward
      , trailInnerBowl
      , trailInnerBeak # backward
      , trailCut
      , trailOuterBeak
      , trailOuterBowl # backward
      ]

narrowBowlWidth :: Given Config => Double
narrowBowlWidth = narrowBowlVirtualWidth - narrowBowlCorrection

xalWidth :: Given Config => Double
xalWidth = narrowBowlVirtualWidth * 2 - thicknessX

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterLeftNarrowBowl :: Given Config => PartTrail
trailOuterLeftNarrowBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2 + overshoot

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、右端から上端への向きで生成します。
-- ただし、他のトレイルと使い方を揃えるため、左右反転してあります。
trailOuterRightNarrowBowl :: Given Config => PartTrail
trailOuterRightNarrowBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2 + overshoot

-- x, j の文字に共通する細い丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerNarrowBowl :: Given Config => PartTrail
trailInnerNarrowBowl = origin ~> (0 &| 25) ~:~ (-width &| 0) <~ (width &| height)
  where
    width = narrowBowlVirtualWidth / 2 - thicknessX
    height = mean / 2 - thicknessY + overshoot

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
    angle = angleBetween (point .-. base) (1 &| 0) ^-^ quarterTurn
    point = head $ closestPoint segment base
    base = (width / 2 &| bend / 2 - thicknessY / 2)
    segment = origin ~> (innerCont &| 0) ~:~ (-outerCont &| 0) <~ (width &| bend)

searchSpineInnerCont :: Given Config => Double -> Double -> Double -> Double
searchSpineInnerCont bend width outerCont = minimumBy (comparing calcSpineError') list
  where
    calcSpineError' innerCont = calcSpineError bend width innerCont outerCont
    list = [0, interval .. width]
    interval = 0.5

-- n の文字の書き終わりの箇所にある曲線を、上端から下端への向きで生成します。
trailNesLeg :: Given Config => PartTrail
trailNesLeg = origin ~> (0 &| -150) ~:~ zero <~ (-bend &| -height)
  where
    bend = nesLegBend
    height = mean / 2

-- n の文字の中央部分の上側の曲線を、下端から上端への向きで生成します。
trailTopSpine :: Given Config => PartTrail
trailTopSpine = origin ~> (leftCont &| 0) ~:~ (-rightCont &| 0) <~ (width &| bend)
  where
    width = spineWidth
    bend = mean - thicknessY + overshoot * 2
    leftCont = searchSpineInnerCont bend width rightCont
    rightCont = width * 1.05

-- n の文字の中央部分の下側の曲線を、下端から上端への向きで生成します。
trailBottomSpine :: Given Config => PartTrail
trailBottomSpine = origin ~> (leftCont &| 0) ~:~ (-rightCont &| 0) <~ (width &| bend)
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
      , trailRightItTail # backward
      , trailInnerBowl
      , trailInnerBeak # backward
      , trailCut
      , trailOuterBeak
      , trailOuterBowl # backward
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
      , trailInnerLink # backward
      , trailInnerBowl
      , trailInnerBeak # backward
      , trailCut
      , trailOuterBeak
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
      , trailInnerAcute # reflectX # backward
      , trailAcuteCut
      , trailOuterAcute # reflectX
      , trailOuterAcute # backward
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

-- バデックの文字の棒状の部分の直線を、上端から下端への向きで生成します。
trailBadekStem :: Given Config => PartTrail
trailBadekStem = origin ~~ (0 &| -height)
  where
    height = ascent - dotWidth - badekGap + overshoot

-- バデックの文字の棒状の部分を生成します。
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

-- パデックの文字の棒状の部分の左側の曲線を、上端から下端への向きで生成します。
trailLeftPadekStem :: Given Config => PartTrail
trailLeftPadekStem = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = dotWidth + dotGap
    height = ascent - dotWidth - badekGap + overshoot
    topCont = searchTailInnerCont bend height bottomCont
    bottomCont = 300

-- パデックの文字の棒状の部分の右側の曲線を、上端から下端への向きで生成します。
trailRightPadekStem :: Given Config => PartTrail
trailRightPadekStem = origin ~> (0 &| -topCont) ~:~ (0 &| bottomCont) <~ (-bend &| -height)
  where
    bend = dotWidth + dotGap
    height = ascent - dotWidth - badekGap + overshoot
    topCont = 300
    bottomCont = searchTailInnerCont bend height topCont

-- パデックの文字の棒状の部分を生成します。
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

-- ノークの文字の棒状の部分の縦の曲線を、上端から下端への向きで生成します。
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