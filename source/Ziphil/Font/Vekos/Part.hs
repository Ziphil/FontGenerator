--


module Ziphil.Font.Vekos.Part
  ( partBowl
  , partTail
  , partLes
  , partYes
  , partTal
  , partNarrowBowl
  , partTransphone
  , tailBend
  , legBend
  , beakHeight
  , narrowBowlWidth
  , narrowBowlOverlap
  , transphoneBend
  )
where

import Diagrams.Prelude
import Ziphil.Font.Util
import Ziphil.Font.Vekos.Param


-- 丸い部分の外側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentOuterBowl :: PartSegment
segmentOuterBowl = bezier3 ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = bowlWidth / 2
    height = mean / 2

-- 丸い部分の内側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentInnerBowl :: PartSegment
segmentInnerBowl = bezier3 ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = bowlWidth / 2 - weightX
    height = mean / 2 - weightY

-- k, p, c, l, a などの文字に共通する丸い部分のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partBowl :: Part
partBowl = mconcat parts # moveOriginBy ~^ (bowlWidth / 2, 0)
  where
    outerSegments =
      [ segmentOuterBowl # transInvert
      , segmentOuterBowl # transTurn # reverseSegment
      , segmentOuterBowl # transReverse
      , segmentOuterBowl # reverseSegment
      ]
    innerSegments =
      [ segmentInnerBowl # transInvert
      , segmentInnerBowl # transTurn # reverseSegment
      , segmentInnerBowl # transReverse
      , segmentInnerBowl # reverseSegment
      ]
    parts =
      [ makePart outerSegments
      , makePart innerSegments # reversePath # translate ~^ (weightX, 0)
      ]

tailBend :: Double
tailBend = bowlWidth / 2

-- c, l などの文字に共通する飛び出す部分の曲線を、上から下の向きで生成します。
segmentTail :: PartSegment
segmentTail = bezier3 ~^ (0, -250) ~^ (-tailBend, -height + 200) ~^ (-tailBend, -height)
  where
    height = mean / 2 + descender

-- 文字の書き始めや書き終わりの位置にある水平に切られた部分を、左から右への向きで生成します。
segmentCut :: PartSegment
segmentCut = straight ~^ (weightX, 0)

-- c, l などの文字に共通するエックスハイトの下に飛び出す部分のパスを生成します。
-- 原点は左上の角にあります。
partTail :: Part
partTail = makePart segments
  where
    segments =
      [ segmentTail
      , segmentCut
      , segmentTail # reverseSegment
      , segmentCut # reverseSegment
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
legBend = bowlWidth / 6

-- y の文字の下半分にある曲線を、上から下への向きで生成します。
segmentLeg :: PartSegment
segmentLeg = bezier3 ~^ (0, -150) ~^ (legBend, -height) ~^ (legBend, -height)
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
      , segmentLeg # reverseSegment
      , segmentInnerBowl
      , segmentInnerBowl # transReverse # reverseSegment
      , segmentLeg # transReverse
      , segmentCut
      , segmentLeg # transReverse # reverseSegment
      , segmentOuterBowl # transReverse
      , segmentOuterBowl # reverseSegment
      ]

beakHeight :: Double
beakHeight = mean / 3

-- t の文字の右上にある部分の外側の曲線を、右端から上端に進む向きで生成します。
segmentOuterBeak :: PartSegment
segmentOuterBeak = bezier3 ~^ (0, 10) ~^ (0, height + overshoot) ~^ (-width, height + overshoot)
  where
    width = bowlWidth / 2
    height = beakHeight

-- t の文字の右上にある部分の内側の曲線を、右端から上端に進む向きで生成します。
segmentInnerBeak :: PartSegment
segmentInnerBeak =  bezier3 ~^ (0, 10) ~^ (0, height + overshoot) ~^ (-width, height + overshoot)
  where
    width = bowlWidth / 2 - weightX
    height = beakHeight - weightY

-- t の文字と同じ形のパスを生成します。
-- 原点は丸い部分の中央にあるので、回転や反転で変化しません。
partTal :: Part
partTal = makePart segments # moveOriginBy ~^ (bowlWidth / 2, 0)
  where
    segments =
      [ segmentOuterBowl # transInvert
      , segmentOuterBeak # transInvert # reverseSegment
      , segmentCut # reverseSegment
      , segmentInnerBeak # transInvert
      , segmentInnerBowl # transInvert # reverseSegment
      , segmentInnerBowl
      , segmentInnerBeak # reverseSegment
      , segmentCut
      , segmentOuterBeak
      , segmentOuterBowl # reverseSegment
      ]

narrowBowlVirtualWidth :: Double
narrowBowlVirtualWidth = bowlWidth * 0.9

narrowBowlCorrection :: Double
narrowBowlCorrection = weightX * 0.15

narrowBowlWidth :: Double
narrowBowlWidth = narrowBowlVirtualWidth - narrowBowlCorrection

narrowBowlOverlap :: Double
narrowBowlOverlap = (weightX - narrowBowlCorrection) * 2 - weightX

-- x などの文字で使われる細い丸い部分の外側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentOuterLeftNarrowBowl :: PartSegment
segmentOuterLeftNarrowBowl = bezier3 ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2
    height = mean / 2

-- x などの文字で使われる細い丸い部分の外側の曲線のうち、右端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentOuterRightNarrowBowl :: PartSegment
segmentOuterRightNarrowBowl = bezier3 ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - narrowBowlCorrection
    height = mean / 2

-- x などの文字で使われる細い丸い部分の内側の曲線のうち、左端から上端に進む全体の 4 分の 1 の曲線を生成します。
segmentInnerNarrowBowl :: PartSegment
segmentInnerNarrowBowl = bezier3 ~^ (0, 25) ~^ (0, height + overshoot) ~^ (width, height + overshoot)
  where
    width = narrowBowlVirtualWidth / 2 - weightX
    height = mean / 2 - weightY

-- x, j などの文字に共通する細い丸い部分のパスを生成します。
-- 2 つ重ねたときに重なった部分が太く見えすぎないように、右側を少し細く補正してあります。
-- 原点は丸い部分の中央にあります。
partNarrowBowl :: Part
partNarrowBowl = mconcat parts # moveOriginBy ~^ (narrowBowlWidth / 2, 0)
  where
    outerSegments =
      [ segmentOuterLeftNarrowBowl # transInvert
      , segmentOuterRightNarrowBowl # transTurn # reverseSegment
      , segmentOuterRightNarrowBowl # transReverse
      , segmentOuterLeftNarrowBowl # reverseSegment
      ]
    innerSegments =
      [ segmentInnerNarrowBowl # transInvert
      , segmentInnerNarrowBowl # transTurn # reverseSegment
      , segmentInnerNarrowBowl # transReverse
      , segmentInnerNarrowBowl # reverseSegment
      ]
    parts =
      [ makePart outerSegments
      , makePart innerSegments # reversePath # translate ~^ (weightX, 0)
      ]

transphoneBend :: Double
transphoneBend = bowlWidth / 6

-- 変音符の外側に向かって曲がる曲線を、上から下の向きで生成します。
segmentTransphone :: PartSegment
segmentTransphone = bezier3 ~^ (0, 0) ~^ (transphoneBend, -height + 150) ~^ (transphoneBend, -height)
  where
    height = mean / 2

-- g, b などの文字に共通する変音符部分のパスを生成します。
-- 原点は左下の角にあります。
partTransphone :: Part
partTransphone = makePart segments # moveOriginBy ~^ (0, -mean)
  where
    segments = 
      [ segmentTransphone
      , segmentTransphone # transInvert # reverseSegment
      , segmentCut
      , segmentTransphone # transInvert
      , segmentTransphone # reverseSegment
      , segmentCut # reverseSegment
      ]

makePart :: [PartSegment] -> Part
makePart = pathFromTrail . closeTrail . fromSegments