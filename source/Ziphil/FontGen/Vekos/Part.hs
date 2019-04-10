--


module Ziphil.FontGen.Vekos.Part
  ( partBowl
  , partTail
  , partLes
  , partYes
  , partTal
  , partNarrowBowl
  , partIt
  , partUpperUt
  , partUtTail
  , partUt
  , partTransphone
  , partAcute
  , tailBend
  , legBend
  , beakWidth
  , beakHeight
  , talWidth
  , narrowBowlVirtualWidth
  , narrowBowlWidth
  , itTailBend
  , linkWidth
  , linkUpperCorrection
  , linkLowerCorrection
  , utTailBend
  , transphoneWeightX
  , transphoneBend
  )
where

import Data.FontGen
import Ziphil.FontGen.Vekos.Param


-- k, p, c, l, a などの文字に共通する丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterBowl :: PartTrail
trailOuterBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = bowlWidth / 2
    height = mean / 2

-- k, p, c, l, a などの文字に共通する丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerBowl :: PartTrail
trailInnerBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = bowlWidth / 2 - weightX
    height = mean / 2 - weightY

-- k, p, c, l, a などの文字に共通する丸い部分を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partBowl :: Part
partBowl = mconcat parts # moveOriginBy ~^ (bowlWidth / 2, 0)
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
      , makePart innerTrails # reversePath # translate ~^ (weightX, 0)
      ]

tailBend :: Double
tailBend = bowlWidth * 0.5

-- l の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightTail :: PartTrail
trailRightTail = bezier3' ~^ (0, -250) ~^ (-tailBend, -height + 200) ~^ (-tailBend, -height)
  where
    height = mean / 2 + descent

-- l の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftTail :: PartTrail
trailLeftTail = bezier3' ~^ (0, -250) ~^ (-tailBend, -height + 200) ~^ (-tailBend, -height)
  where
    height = mean / 2 + descent

-- 文字の書き始めや書き終わりの位置にある水平に切られた部分を、左端から右端への向きで生成します。
trailCut :: PartTrail
trailCut = straight' ~^ (weightX, 0)

-- l の文字のディセンダーを生成します。
-- 反転や回転を施すことで、c などの文字のディセンダーや k, p などの文字のアセンダーとしても使えます。
-- 原点は左上の角にあります。
partTail :: Part
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
partLes :: Part
partLes = mconcat parts
  where
    parts =
      [ partBowl
      , partTail # translate ~^ (bowlWidth / 2 - weightX, 0)
      ]

legBend :: Double
legBend = bowlWidth * 0.15

-- y の文字の下半分にある曲線を、上端から下端への向きで生成します。
trailLeg :: PartTrail
trailLeg = bezier3' ~^ (0, -150) ~^ (legBend, -height) ~^ (legBend, -height)
  where
    height = mean / 2

-- y の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partYes :: Part
partYes = makePart trails # moveOriginBy ~^ (bowlWidth / 2, 0)
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

beakWidth :: Double
beakWidth = bowlWidth / 2 * 0.95
  
beakHeight :: Double
beakHeight = mean * 0.35

talWidth :: Double
talWidth = bowlWidth / 2 + beakWidth

-- t の文字の右上にある部分の外側の曲線を、右端から上端への向きで生成します。
trailOuterBeak :: PartTrail
trailOuterBeak = bezier3' ~^ (0, 10) ~^ (0, height + overshoot) ~^ (-width, height + overshoot)
  where
    width = beakWidth
    height = beakHeight

-- t の文字の右上にある部分の内側の曲線を、右端から上端への向きで生成します。
trailInnerBeak :: PartTrail
trailInnerBeak =  bezier3' ~^ (0, 10) ~^ (0, height + overshoot) ~^ (-width, height + overshoot)
  where
    width = beakWidth - weightX
    height = beakHeight - weightY

-- t の文字と同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partTal :: Part
partTal = makePart trails # moveOriginBy ~^ (talWidth / 2, 0)
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

narrowBowlVirtualWidth :: Double
narrowBowlVirtualWidth = bowlWidth * 0.9

narrowBowlCorrection :: Double
narrowBowlCorrection = weightX * 0.15

narrowBowlWidth :: Double
narrowBowlWidth = narrowBowlVirtualWidth - narrowBowlCorrection

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailOuterLeftNarrowBowl :: PartTrail
trailOuterLeftNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2

-- x, j の文字に共通する細い丸い部分の外側の曲線の 4 分の 1 を、右端から上端への向きで生成します。
-- ただし、他のトレイルと使い方を揃えるため、左右反転してあります。
trailOuterRightNarrowBowl :: PartTrail
trailOuterRightNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2

-- x, j の文字に共通する細い丸い部分の内側の曲線の 4 分の 1 を、左端から上端への向きで生成します。
trailInnerNarrowBowl :: PartTrail
trailInnerNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - weightX
    height = mean / 2 - weightY

-- x, j の文字に共通する細い丸い部分を生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は全体の中央にあります。
partNarrowBowl :: Part
partNarrowBowl = mconcat parts # moveOriginBy ~^ (narrowBowlVirtualWidth / 2, 0)
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
      , makePart innerTrails # reversePath # translate ~^ (weightX, 0)
      ]

itTailBend :: Double
itTailBend = bowlWidth * 0.5

-- i の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftItTail :: PartTrail
trailLeftItTail = bezier3' ~^ (0, -250) ~^ (itTailBend, -height + 300) ~^ (itTailBend, -height)
  where
    height = mean / 2 + descent

-- i の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightItTail :: PartTrail
trailRightItTail = bezier3' ~^ (0, -210) ~^ (itTailBend, -height + 325) ~^ (itTailBend, -height)
  where
    height = mean / 2 + descent

-- i の文字と同じ形を生成します。
-- 原点は上部の丸い部分の中央にあるので、回転や反転で変化しません。
partIt :: Part
partIt = makePart trails # moveOriginBy ~^ (talWidth / 2, 0)
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

linkWidth :: Double
linkWidth = bowlWidth * 0.8

linkUpperCorrection :: Double
linkUpperCorrection = weightY * 0.1

linkLowerCorrection :: Double
linkLowerCorrection = weightY * 0.1

utTailBend :: Double
utTailBend = bowlWidth * 0.55

-- u の文字のディセンダーと接続する部分の外側の曲線を、上端から下端への向きで生成します。
trailOuterLink :: PartTrail
trailOuterLink = bezier3' ~^ (0, 25) ~^ (0, -height) ~^ (width, -height)
  where
    width = linkWidth
    height = mean / 2 - linkLowerCorrection

-- u の文字のディセンダーと接続する部分の内側の曲線を、上端から下端への向きで生成します。
trailInnerLink :: PartTrail
trailInnerLink = bezier3' ~^ (0, 25) ~^ (0, -height) ~^ (width, -height)
  where
    width = linkWidth - weightX
    height = mean / 2 - weightY

-- u の文字のディセンダーの左側の曲線を、上端から下端への向きで生成します。
trailLeftUtTail :: PartTrail
trailLeftUtTail = bezier3' ~^ (0, 25) ~^ (0, height) ~^ (utTailBend, height)
  where
    height = descent + weightY - linkUpperCorrection

-- u の文字のディセンダーの右側の曲線を、上端から下端への向きで生成します。
trailRightUtTail :: PartTrail
trailRightUtTail = bezier3' ~^ (0, 25) ~^ (0, height) ~^ (utTailBend - weightX, height)
  where
    height = descent

-- u の文字のベースラインより上にある丸い部分を生成します。
-- ディセンダーと重ねたときに太く見えすぎないように、下側を少し細く補正してあります。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partUpperUt :: Part
partUpperUt = makePart trails # moveOriginBy ~^ (talWidth / 2, 0)
  where
    trails =
      [ trailOuterLink
      , straight' ~^ (0, weightY - linkLowerCorrection)
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
partUtTail :: Part
partUtTail = makePart trails
  where
    trails =
      [ trailLeftUtTail # reverseTrail
      , trailCut
      , trailRightUtTail
      , straight' ~^ (0, weightY - linkUpperCorrection)
      ]

-- u の文字と同じ形を生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partUt :: Part
partUt = mconcat parts
  where
    parts =
      [ partUpperUt
      , partUtTail # translate ~^ (-talWidth / 2 + linkWidth, -mean / 2 + weightY - linkUpperCorrection)
      ]

transphoneWeightX :: Double
transphoneWeightX = weightX * 0.95

transphoneBend :: Double
transphoneBend = bowlWidth * 0.15

-- 変音符の右に飛び出るように曲がる曲線の上半分を、上端から下端への向きで生成します。
trailTransphone :: PartTrail
trailTransphone = bezier3' ~^ (0, 0) ~^ (transphoneBend, -height + 150) ~^ (transphoneBend, -height)
  where
    height = mean / 2

-- 変音符の上下にある水平に切られた部分を、左端から右端への向きで生成します。
trailTransphoneCut :: PartTrail
trailTransphoneCut = straight' ~^ (transphoneWeightX, 0)

-- 変音符と同じ形を生成します。
-- 原点は左下の角にあります。
partTransphone :: Part
partTransphone = makePart trails # moveOriginBy ~^ (0, -mean)
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
trailOuterAcute :: PartTrail
trailOuterAcute = bezier3' ~^ (0, 10) ~^ (0, height) ~^ (width, height)
  where
    width = diacriticWidth / 2
    height = diacriticHeight
    
-- アキュートアクセントの丸い部分の内側の曲線の半分を、左下端から上端への向きで生成します。
trailInnerAcute :: PartTrail
trailInnerAcute = bezier3' ~^ (0, 10) ~^ (0, height) ~^ (width, height)
  where
    width = diacriticWidth / 2 - diacriticWeightX
    height = diacriticHeight - diacriticWeightY

-- アキュートアクセントの下部にある水平に切られた部分を、左端から右端への向きで生成します。
trailAcuteCut :: PartTrail
trailAcuteCut = straight' ~^ (diacriticWeightX, 0)

-- アキュートアクセントと同じ形を生成します。
-- 原点は全体の中央にあるので、回転や反転で変化しません。
partAcute :: Part
partAcute = makePart trails # moveOriginBy ~^ (diacriticWidth / 2, diacriticHeight / 2)
  where
    trails =
      [ trailAcuteCut
      , trailInnerAcute
      , trailInnerAcute # reflectX # reverseTrail
      , trailAcuteCut
      , trailOuterAcute # reflectX
      , trailOuterAcute # reverseTrail
      ]

makePart :: [PartTrail] -> Part
makePart = pathFromTrail . closeTrail . mconcat