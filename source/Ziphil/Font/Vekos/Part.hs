--


module Ziphil.Font.Vekos.Part
  ( partBowl
  , partTail
  , partLes
  , partYes
  , partTal
  , partNarrowBowl
  , partIt
  , partTransphone
  , tailBend
  , legBend
  , beakWidth
  , beakHeight
  , talWidth
  , narrowBowlVirtualWidth
  , narrowBowlWidth
  , itTailBend
  , transphoneWeightX
  , transphoneBend
  )
where

import Diagrams.Prelude
import Diagrams.TwoD.Offset
import Ziphil.Font.Util
import Ziphil.Font.Vekos.Param


-- 丸い部分の外側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
trailOuterBowl :: PartTrail
trailOuterBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = bowlWidth / 2
    height = mean / 2

-- 丸い部分の内側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
trailInnerBowl :: PartTrail
trailInnerBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = bowlWidth / 2 - weightX
    height = mean / 2 - weightY

-- k, p, c, l, a などの文字に共通する丸い部分のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
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

-- c, l などの文字に共通する飛び出す部分の曲線を、上から下の向きで生成します。
trailTail :: PartTrail
trailTail = bezier3' ~^ (0, -250) ~^ (-tailBend, -height + 200) ~^ (-tailBend, -height)
  where
    height = mean / 2 + descent

-- 文字の書き始めや書き終わりの位置にある水平に切られた部分を、左から右への向きで生成します。
trailCut :: PartTrail
trailCut = straight' ~^ (weightX, 0)

-- c, l などの文字に共通するエックスハイトの下に飛び出す部分のパスを生成します。
-- 原点は左上の角にあります。
partTail :: Part
partTail = makePart trails
  where
    trails =
      [ trailTail
      , trailCut
      , trailTail # reverseTrail
      , trailCut # reverseTrail
      ]

-- l の文字と同じ形のパスを生成します。
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

-- y の文字の下半分にある曲線を、上から下への向きで生成します。
trailLeg :: PartTrail
trailLeg = bezier3' ~^ (0, -150) ~^ (legBend, -height) ~^ (legBend, -height)
  where
    height = mean / 2

-- y の文字と同じ形のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
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

-- t の文字の右上にある部分の外側の曲線を、右端から上端に進む向きで生成します。
trailOuterBeak :: PartTrail
trailOuterBeak = bezier3' ~^ (0, 10) ~^ (0, height + overshoot) ~^ (-width, height + overshoot)
  where
    width = beakWidth
    height = beakHeight

-- t の文字の右上にある部分の内側の曲線を、右端から上端に進む向きで生成します。
trailInnerBeak :: PartTrail
trailInnerBeak =  bezier3' ~^ (0, 10) ~^ (0, height + overshoot) ~^ (-width, height + overshoot)
  where
    width = beakWidth - weightX
    height = beakHeight - weightY

-- t の文字と同じ形のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
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

-- x などの文字で使われる細い丸い部分の外側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
trailOuterLeftNarrowBowl :: PartTrail
trailOuterLeftNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2

-- x などの文字で使われる細い丸い部分の外側の曲線のうち、右端から上端に進む全体の 4 分の 1 の曲線を生成します。
trailOuterRightNarrowBowl :: PartTrail
trailOuterRightNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2

-- x などの文字で使われる細い丸い部分の内側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
trailInnerNarrowBowl :: PartTrail
trailInnerNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - weightX
    height = mean / 2 - weightY

-- x, j などの文字に共通する細い丸い部分のパスを生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は丸い部分の中央にあります。
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

-- i の文字に含まれる飛び出す部分の左側の曲線を、上から下の向きで生成します。
trailLeftItTail :: PartTrail
trailLeftItTail = bezier3' ~^ (0, -250) ~^ (itTailBend, -height + 300) ~^ (itTailBend, -height)
  where
    height = mean / 2 + descent

-- i の文字に含まれる飛び出す部分の右側の曲線を、上から下の向きで生成します。
trailRightItTail :: PartTrail
trailRightItTail = bezier3' ~^ (0, -210) ~^ (itTailBend, -height + 325) ~^ (itTailBend, -height)
  where
    height = mean / 2 + descent

-- i の文字と同じ形のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
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

transphoneWeightX :: Double
transphoneWeightX = weightX * 0.95

transphoneBend :: Double
transphoneBend = bowlWidth * 0.15

-- 変音符の外側に向かって曲がる曲線を、上から下の向きで生成します。
trailTransphone :: PartTrail
trailTransphone = bezier3' ~^ (0, 0) ~^ (transphoneBend, -height + 150) ~^ (transphoneBend, -height)
  where
    height = mean / 2

-- 変音符の上下にある水平に切られた部分を、左から右への向きで生成します。
trailTransphoneCut :: PartTrail
trailTransphoneCut = straight' ~^ (transphoneWeightX, 0)

-- g, b などの文字に共通する変音符部分のパスを生成します。
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

makePart :: [PartTrail] -> Part
makePart = pathFromTrail . closeTrail . mconcat