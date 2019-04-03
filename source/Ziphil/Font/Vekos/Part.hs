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
segmentOuterBowl :: PartTrail
segmentOuterBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = bowlWidth / 2
    height = mean / 2

-- 丸い部分の内側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentInnerBowl :: PartTrail
segmentInnerBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = bowlWidth / 2 - weightX
    height = mean / 2 - weightY

-- k, p, c, l, a などの文字に共通する丸い部分のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partBowl :: Part
partBowl = mconcat parts # moveOriginBy ~^ (bowlWidth / 2, 0)
  where
    outerSegments =
      [ segmentOuterBowl # reflectY
      , segmentOuterBowl # rotateHalfTurn # reverseTrail
      , segmentOuterBowl # reflectX
      , segmentOuterBowl # reverseTrail
      ]
    innerSegments =
      [ segmentInnerBowl # reflectY
      , segmentInnerBowl # rotateHalfTurn # reverseTrail
      , segmentInnerBowl # reflectX
      , segmentInnerBowl # reverseTrail
      ]
    parts =
      [ makePart outerSegments
      , makePart innerSegments # reversePath # translate ~^ (weightX, 0)
      ]

tailBend :: Double
tailBend = bowlWidth * 0.5

-- c, l などの文字に共通する飛び出す部分の曲線を、上から下の向きで生成します。
segmentTail :: PartTrail
segmentTail = bezier3' ~^ (0, -250) ~^ (-tailBend, -height + 200) ~^ (-tailBend, -height)
  where
    height = mean / 2 + descent

-- 文字の書き始めや書き終わりの位置にある水平に切られた部分を、左から右への向きで生成します。
segmentCut :: PartTrail
segmentCut = straight' ~^ (weightX, 0)

-- c, l などの文字に共通するエックスハイトの下に飛び出す部分のパスを生成します。
-- 原点は左上の角にあります。
partTail :: Part
partTail = makePart segments
  where
    segments =
      [ segmentTail
      , segmentCut
      , segmentTail # reverseTrail
      , segmentCut # reverseTrail
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
segmentLeg :: PartTrail
segmentLeg = bezier3' ~^ (0, -150) ~^ (legBend, -height) ~^ (legBend, -height)
  where
    height = mean / 2

-- y の文字と同じ形のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partYes :: Part
partYes = makePart segments # moveOriginBy ~^ (bowlWidth / 2, 0)
  where
    segments =
      [ segmentLeg
      , segmentCut
      , segmentLeg # reverseTrail
      , segmentInnerBowl
      , segmentInnerBowl # reflectX # reverseTrail
      , segmentLeg # reflectX
      , segmentCut
      , segmentLeg # reflectX # reverseTrail
      , segmentOuterBowl # reflectX
      , segmentOuterBowl # reverseTrail
      ]

beakWidth :: Double
beakWidth = bowlWidth / 2 * 0.95
  
beakHeight :: Double
beakHeight = mean * 0.35

talWidth :: Double
talWidth = bowlWidth / 2 + beakWidth

-- t の文字の右上にある部分の外側の曲線を、右端から上端に進む向きで生成します。
segmentOuterBeak :: PartTrail
segmentOuterBeak = bezier3' ~^ (0, 10) ~^ (0, height + overshoot) ~^ (-width, height + overshoot)
  where
    width = beakWidth
    height = beakHeight

-- t の文字の右上にある部分の内側の曲線を、右端から上端に進む向きで生成します。
segmentInnerBeak :: PartTrail
segmentInnerBeak =  bezier3' ~^ (0, 10) ~^ (0, height + overshoot) ~^ (-width, height + overshoot)
  where
    width = beakWidth - weightX
    height = beakHeight - weightY

-- t の文字と同じ形のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partTal :: Part
partTal = makePart segments # moveOriginBy ~^ (talWidth / 2, 0)
  where
    segments =
      [ segmentOuterBowl # reflectY
      , segmentOuterBeak # reflectY # reverseTrail
      , segmentCut # reverseTrail
      , segmentInnerBeak # reflectY
      , segmentInnerBowl # reflectY # reverseTrail
      , segmentInnerBowl
      , segmentInnerBeak # reverseTrail
      , segmentCut
      , segmentOuterBeak
      , segmentOuterBowl # reverseTrail
      ]

narrowBowlVirtualWidth :: Double
narrowBowlVirtualWidth = bowlWidth * 0.9

narrowBowlCorrection :: Double
narrowBowlCorrection = weightX * 0.15

narrowBowlWidth :: Double
narrowBowlWidth = narrowBowlVirtualWidth - narrowBowlCorrection

-- x などの文字で使われる細い丸い部分の外側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentOuterLeftNarrowBowl :: PartTrail
segmentOuterLeftNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2

-- x などの文字で使われる細い丸い部分の外側の曲線のうち、右端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentOuterRightNarrowBowl :: PartTrail
segmentOuterRightNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2

-- x などの文字で使われる細い丸い部分の内側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentInnerNarrowBowl :: PartTrail
segmentInnerNarrowBowl = bezier3' ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - weightX
    height = mean / 2 - weightY

-- x, j などの文字に共通する細い丸い部分のパスを生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は丸い部分の中央にあります。
partNarrowBowl :: Part
partNarrowBowl = mconcat parts # moveOriginBy ~^ (narrowBowlVirtualWidth / 2, 0)
  where
    outerSegments =
      [ segmentOuterLeftNarrowBowl # reflectY
      , segmentOuterRightNarrowBowl # rotateHalfTurn # reverseTrail
      , segmentOuterRightNarrowBowl # reflectX
      , segmentOuterLeftNarrowBowl # reverseTrail
      ]
    innerSegments =
      [ segmentInnerNarrowBowl # reflectY
      , segmentInnerNarrowBowl # rotateHalfTurn # reverseTrail
      , segmentInnerNarrowBowl # reflectX
      , segmentInnerNarrowBowl # reverseTrail
      ]
    parts =
      [ makePart outerSegments
      , makePart innerSegments # reversePath # translate ~^ (weightX, 0)
      ]

itTailBend :: Double
itTailBend = bowlWidth * 0.5

-- i の文字に含まれる飛び出す部分の左側の曲線を、上から下の向きで生成します。
segmentLeftItTail :: PartTrail
segmentLeftItTail = bezier3' ~^ (0, -250) ~^ (itTailBend, -height + 300) ~^ (itTailBend, -height)
  where
    height = mean / 2 + descent

-- i の文字に含まれる飛び出す部分の右側の曲線を、上から下の向きで生成します。
segmentRightItTail :: PartTrail
segmentRightItTail = bezier3' ~^ (0, -210) ~^ (itTailBend, -height + 325) ~^ (itTailBend, -height)
  where
    height = mean / 2 + descent

-- i の文字と同じ形のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partIt :: Part
partIt = makePart segments # moveOriginBy ~^ (talWidth / 2, 0)
  where
    segments =
      [ segmentLeftItTail
      , segmentCut
      , segmentRightItTail # reverseTrail
      , segmentInnerBowl
      , segmentInnerBeak # reverseTrail
      , segmentCut
      , segmentOuterBeak
      , segmentOuterBowl # reverseTrail
      ]

transphoneWeightX :: Double
transphoneWeightX = weightX * 0.95

transphoneBend :: Double
transphoneBend = bowlWidth * 0.15

-- 変音符の外側に向かって曲がる曲線を、上から下の向きで生成します。
segmentTransphone :: PartTrail
segmentTransphone = bezier3' ~^ (0, 0) ~^ (transphoneBend, -height + 150) ~^ (transphoneBend, -height)
  where
    height = mean / 2

-- 変音符の上下にある水平に切られた部分を、左から右への向きで生成します。
segmentTransphoneCut :: PartTrail
segmentTransphoneCut = straight' ~^ (transphoneWeightX, 0)

-- g, b などの文字に共通する変音符部分のパスを生成します。
-- 原点は左下の角にあります。
partTransphone :: Part
partTransphone = makePart segments # moveOriginBy ~^ (0, -mean)
  where
    segments = 
      [ segmentTransphone
      , segmentTransphone # reflectY # reverseTrail
      , segmentTransphoneCut
      , segmentTransphone # reflectY
      , segmentTransphone # reverseTrail
      , segmentTransphoneCut # reverseTrail
      ]

makePart :: [PartTrail] -> Part
makePart = pathFromTrail . closeTrail . mconcat