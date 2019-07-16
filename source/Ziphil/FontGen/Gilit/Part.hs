{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Part
  ( partLeftOblique
  , partRightOblique
  , partLeftChippedOblique
  , partRightChippedOblique
  , partBase
  , partChippedBase
  , partLeftAscender
  , partRightAscender
  , partLeftDescender
  , partRightDescender
  , obliqueAngle
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
import Ziphil.FontGen.Gilit.Value


obliqueAngle :: Given Config => Angle Double
obliqueAngle = atan2A triangleHeight (triangleWidth / 2)

trailBottomLeftOblique :: Given Config => PartTrail
trailBottomLeftOblique = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle

trailRightLeftOblique :: Given Config => PartTrail
trailRightLeftOblique = origin ~~ (x &| y)
  where
    x = y / (tanA obliqueAngle)
    y = triangleHeight + thickness / 2

trailTopLeftOblique :: Given Config => PartTrail
trailTopLeftOblique = origin ~~ (x &| y)
  where
    x = -thickness / (sinA obliqueAngle * 2)
    y = thickness / (cosA obliqueAngle * 2)

trailLeftLeftOblique :: Given Config => PartTrail
trailLeftLeftOblique = origin ~~ (x &| y)
  where
    x = y / (tanA obliqueAngle)
    y = -triangleHeight - thickness / (cosA obliqueAngle * 2) - thickness / 2

partLeftOblique :: Given Config => Part
partLeftOblique = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailBottomLeftOblique
      , trailRightLeftOblique
      , trailTopLeftOblique
      , trailLeftLeftOblique
      ]
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightOblique :: Given Config => Part
partRightOblique = partLeftOblique # reflectX # moveOriginBy (originX &| 0)
  where
    originX = -triangleWidth

trailBottomLeftChippedOblique :: Given Config => PartTrail
trailBottomLeftChippedOblique = origin ~~ (x &| 0)
  where
    x = thickness / (sinA obliqueAngle) - thickness / (tanA obliqueAngle)

trailLeftLeftChippedOblique :: Given Config => PartTrail
trailLeftLeftChippedOblique = origin ~~ (x &| y)
  where
    x = y / (tanA obliqueAngle)
    y = -triangleHeight - thickness / (cosA obliqueAngle * 2)

trailChip :: Given Config => PartTrail
trailChip = origin ~~ (x &| y)
  where
    x = -y / (tanA obliqueAngle)
    y = -thickness / 2

partLeftChippedOblique :: Given Config => Part
partLeftChippedOblique = makePart trails # moveOriginBy (originX &| 0) 
  where
    trails =
      [ trailChip
      , trailBottomLeftChippedOblique
      , trailRightLeftOblique
      , trailTopLeftOblique
      , trailLeftLeftChippedOblique
      ]
    originX = thickness / (tanA obliqueAngle)

partRightChippedOblique :: Given Config => Part
partRightChippedOblique = partLeftChippedOblique # reflectX # moveOriginBy (-triangleWidth &| 0)

trailBottomBase :: Given Config => PartTrail
trailBottomBase = origin ~~ (x &| 0)
  where
    x = triangleWidth + thickness / (sinA obliqueAngle) + thickness / (tanA obliqueAngle)

trailRightBase :: Given Config => PartTrail
trailRightBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = thickness

trailTopBase :: Given Config => PartTrail
trailTopBase = origin ~~ (x &| 0)
  where
    x = -triangleWidth - thickness / (sinA obliqueAngle) + thickness / (tanA obliqueAngle)

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

trailBottomLeftAscender :: Given Config => PartTrail
trailBottomLeftAscender = origin ~~ (x &| y)
  where
    x = thickness / (sinA obliqueAngle * 2)
    y = thickness / (cosA obliqueAngle * 2)

trailRightLeftAscender :: Given Config => PartTrail
trailRightLeftAscender = origin ~~ (x &| y)
  where
    x = -y / (tanA obliqueAngle)
    y = ascenderHeight + thickness / (cosA obliqueAngle * 4)

partLeftAscender :: Given Config => Part
partLeftAscender = makePart trails # moveOriginBy (originX &| originY)
  where
    trails =
      [ trailBottomLeftAscender
      , trailRightLeftAscender
      , trailBottomLeftAscender # backward
      , trailRightLeftAscender # backward
      ]
    originX = -triangleWidth / 2
    originY = -triangleHeight + thickness / (cosA obliqueAngle * 2)

partRightAscender :: Given Config => Part
partRightAscender = partLeftAscender # reflectX # moveOriginBy (-triangleWidth &| 0)

trailRightLeftDescender :: Given Config => PartTrail
trailRightLeftDescender = origin ~~ (x &| y)
  where
    x = -y / (tanA obliqueAngle)
    y = ascenderHeight + thickness / (cosA obliqueAngle * 4)

partLeftDescender :: Given Config => Part
partLeftDescender = makePart trails # moveOriginBy (originX &| 0)
  where
    trails = 
      [ trailRightLeftDescender # backward
      , trailBottomLeftAscender
      , trailRightLeftDescender
      , trailBottomLeftAscender # backward
      ]
    originX = thickness / (sinA obliqueAngle * 2)

partRightDescender :: Given Config => Part
partRightDescender = partLeftDescender # reflectX # moveOriginBy (-triangleWidth &| 0)

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