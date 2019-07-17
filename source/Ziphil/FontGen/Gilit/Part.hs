{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Part
  ( partLeftOblique
  , partRightOblique
  , partLeftShortOblique
  , partRightShortOblique
  , partLeftChippedOblique
  , partRightChippedOblique
  , partBase
  , partLeftChippedBase
  , partRightChippedBase
  , partChippedBase
  , partLeftAscender
  , partRightAscender
  , partLeftDescender
  , partRightDescender
  , partLeftChippedDescender
  , partRightChippedDescender
  , partTransphone
  , partDiamond
  , obliqueAngle
  , horizontalTransphoneGap
  , horizontalGap
  , defaultLeftX
  , widthDifference
  , ascent
  , descent
  , em
  , actualAscent
  , actualDescent
  , actualEm
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.PartFunc
import Ziphil.FontGen.Gilit.Value


obliqueAngle :: Given Config => Angle Double
obliqueAngle = atan2A triangleHeight (triangleWidth / 2)

trailCut :: Given Config => PartTrail
trailCut = origin ~~ (x &| y)
  where
    x = thickness / (sinA obliqueAngle * 2)
    y = thickness / (cosA obliqueAngle * 2)

trailHorizontalCut :: Given Config => PartTrail
trailHorizontalCut = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle

trailRightLeftOblique :: Given Config => PartTrail
trailRightLeftOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = triangleHeight + thickness / 2

trailLeftLeftOblique :: Given Config => PartTrail
trailLeftLeftOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight - thickness / (cosA obliqueAngle * 2) - thickness / 2

partLeftOblique :: Given Config => Part
partLeftOblique = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailHorizontalCut
      , trailRightLeftOblique
      , trailCut # reflectY # backward
      , trailLeftLeftOblique
      ]
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightOblique :: Given Config => Part
partRightOblique = partLeftOblique # reflectSide

trailRightLeftShortOblique :: Given Config => PartTrail
trailRightLeftShortOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = triangleHeight - diamondGap * sinA obliqueAngle + thickness / 2 - thickness / (cosA obliqueAngle * 2)

trailLeftLeftShortOblique :: Given Config => PartTrail
trailLeftLeftShortOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight + diamondGap * sinA obliqueAngle - thickness / 2

partLeftShortOblique :: Given Config => Part
partLeftShortOblique = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailHorizontalCut
      , trailRightLeftShortOblique
      , trailCut # reflectY # backward
      , trailLeftLeftShortOblique
      ]
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightShortOblique :: Given Config => Part
partRightShortOblique = partLeftShortOblique # reflectSide

trailBottomLeftChippedOblique :: Given Config => PartTrail
trailBottomLeftChippedOblique = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle - thickness / tanA obliqueAngle

trailLeftLeftChippedOblique :: Given Config => PartTrail
trailLeftLeftChippedOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight - thickness / (cosA obliqueAngle * 2)

trailChip :: Given Config => PartTrail
trailChip = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -thickness / 2

partLeftChippedOblique :: Given Config => Part
partLeftChippedOblique = makePart trails # moveOriginBy (originX &| 0) 
  where
    trails =
      [ trailChip
      , trailBottomLeftChippedOblique
      , trailRightLeftOblique
      , trailCut # reflectY # backward
      , trailLeftLeftChippedOblique
      ]
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedOblique :: Given Config => Part
partRightChippedOblique = partLeftChippedOblique # reflectSide

trailBottomBase :: Given Config => PartTrail
trailBottomBase = origin ~~ (x &| 0)
  where
    x = triangleWidth + thickness / sinA obliqueAngle + thickness / tanA obliqueAngle

trailRightBase :: Given Config => PartTrail
trailRightBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = thickness

trailTopBase :: Given Config => PartTrail
trailTopBase = origin ~~ (x &| 0)
  where
    x = -triangleWidth - thickness / sinA obliqueAngle + thickness / tanA obliqueAngle

trailLeftBase :: Given Config => PartTrail
trailLeftBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = -thickness

partBase :: Given Config => Part
partBase = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailBottomBase
      , trailRightBase
      , trailTopBase
      , trailLeftBase
      ]
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

trailBottomLeftChippedBase :: Given Config => PartTrail
trailBottomLeftChippedBase = origin ~~ (x &| 0)
  where
    x = triangleWidth + thickness / sinA obliqueAngle

partLeftChippedBase :: Given Config => Part
partLeftChippedBase = makePart trails # moveOriginBy (originX &| 0)
  where
    trails =
      [ trailChip
      , trailBottomLeftChippedBase
      , trailRightBase
      , trailTopBase
      , trailChip # reflectY # backward
      ]
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedBase :: Given Config => Part
partRightChippedBase = partLeftChippedBase # reflectSide

partChippedBase :: Given Config => Part
partChippedBase = makePart trails # moveOriginBy (originX &| 0)
  where
    trails =
      [ trailChip
      , trailTopBase # backward
      , trailChip # reflectX # backward
      , trailChip # rotateHalfTurn
      , trailTopBase
      , trailChip # reflectY # backward
      ]
    originX = thickness / (sinA obliqueAngle * 2)

trailRightLeftAscender :: Given Config => PartTrail
trailRightLeftAscender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = ascenderHeight + thickness / (cosA obliqueAngle * 4)

partLeftAscender :: Given Config => Part
partLeftAscender = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailCut
      , trailRightLeftAscender
      , trailCut # backward
      , trailRightLeftAscender # backward
      ]
    originX = -triangleWidth / 2
    originY = -triangleHeight + thickness / (cosA obliqueAngle * 2)

partRightAscender :: Given Config => Part
partRightAscender = partLeftAscender # reflectSide

trailLeftLeftDescender :: Given Config => PartTrail
trailLeftLeftDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -ascenderHeight - thickness / 2 - thickness / (cosA obliqueAngle * 4)

trailRightLeftDescender :: Given Config => PartTrail
trailRightLeftDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = ascenderHeight + thickness / 2 - thickness / (cosA obliqueAngle * 4)

partLeftDescender :: Given Config => Part
partLeftDescender = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailLeftLeftDescender
      , trailCut
      , trailRightLeftDescender
      , trailHorizontalCut # backward
      ]
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = -thickness / 2

partRightDescender :: Given Config => Part
partRightDescender = partLeftDescender # reflectSide

trailLeftLeftChippedDescender :: Given Config => PartTrail
trailLeftLeftChippedDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -ascenderHeight - thickness / (cosA obliqueAngle * 4)

partLeftChippedDescender :: Given Config => Part
partLeftChippedDescender = makePart trails # moveOriginBy (originX &| 0)
  where
    trails = 
      [ trailLeftLeftChippedDescender
      , trailCut
      , trailRightLeftDescender
      , trailBottomLeftChippedOblique # backward
      , trailChip
      ]
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedDescender :: Given Config => Part
partRightChippedDescender = partLeftChippedDescender # reflectSide

horizontalTransphoneGap :: Given Config => Double
horizontalTransphoneGap = transphoneGap / sinA obliqueAngle

trailRightTransphone :: Given Config => PartTrail
trailRightTransphone = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = triangleHeight + thickness

partTransphone :: Given Config => Part
partTransphone = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailHorizontalCut
      , trailRightTransphone
      , trailHorizontalCut # backward
      , trailRightTransphone # backward
      ]
    originX = -triangleWidth - horizontalTransphoneGap - thickness / (sinA obliqueAngle * 2) - thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partDiamond :: Given Config => Part
partDiamond = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailCut
      , trailCut # reflectY # backward
      , trailCut # rotateHalfTurn
      , trailCut # reflectX # backward
      ]
    originX = -triangleWidth / 2
    originY = -triangleHeight + thickness / (cosA obliqueAngle * 2)

horizontalGap :: Given Config => Double
horizontalGap = gap / sinA obliqueAngle

defaultLeftX :: Given Config => Double
defaultLeftX = triangleWidth / 4 - horizontalGap / 2 - thickness / (sinA obliqueAngle * 2)

widthDifference :: Given Config => Double
widthDifference = triangleWidth / 2 - horizontalGap - thickness / (sinA obliqueAngle)

ascent :: Given Config => Double
ascent = triangleHeight + ascenderHeight + thickness / (cosA obliqueAngle * 4)

descent :: Given Config => Double
descent = ascenderHeight + thickness / (cosA obliqueAngle * 4)

em :: Given Config => Double
em = ascent + descent

actualAscent :: Given Config => Double
actualAscent = ascent + 20

actualDescent :: Given Config => Double
actualDescent = descent + 20

actualEm :: Given Config => Double
actualEm = actualDescent + actualAscent