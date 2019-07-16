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
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.Util
import Ziphil.FontGen.Gilit.Value


obliqueAngle :: Given Config => Angle Double
obliqueAngle = atan2A (mean + overshoot) (triangleWidth / 2)

trailBottomLeftOblique :: Given Config => PartTrail
trailBottomLeftOblique = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle

trailRightLeftOblique :: Given Config => PartTrail
trailRightLeftOblique = origin ~~ (x &| y)
  where
    x = triangleWidth / 2 - thickness / (sinA obliqueAngle * 2)
    y = mean + overshoot - thickness / (cosA obliqueAngle * 2)

trailTopLeftOblique :: Given Config => PartTrail
trailTopLeftOblique = origin ~~ (x &| y)
  where
    x = -thickness / (sinA obliqueAngle * 2)
    y = thickness / (cosA obliqueAngle * 2)

trailLeftLeftOblique :: Given Config => PartTrail
trailLeftLeftOblique = origin ~~ (x &| y)
  where
    x = -triangleWidth / 2
    y = -mean - overshoot

partLeftOblique :: Given Config => Part
partLeftOblique = makePart trails
  where
    trails =
      [ trailBottomLeftOblique
      , trailRightLeftOblique
      , trailTopLeftOblique
      , trailLeftLeftOblique
      ]

partRightOblique :: Given Config => Part
partRightOblique = partLeftOblique # reflectX # moveOriginBy (-triangleWidth &| 0)

trailBottomLeftChippedOblique :: Given Config => PartTrail
trailBottomLeftChippedOblique = origin ~~ (x &| 0)
  where
    x = thickness / (sinA obliqueAngle) - thickness / (tanA obliqueAngle)

trailLeftLeftChippedOblique :: Given Config => PartTrail
trailLeftLeftChippedOblique = origin ~~ (x &| y)
  where
    x = -triangleWidth / 2 + thickness / (tanA obliqueAngle * 2)
    y = -mean - overshoot + thickness / 2

trailChip :: Given Config => PartTrail
trailChip = origin ~~ (x &| y)
  where
    x = thickness / (tanA obliqueAngle * 2)
    y = -thickness / 2

partLeftChippedOblique :: Given Config => Part
partLeftChippedOblique = makePart trails # moveOriginBy (originX &| 0) 
  where
    trails =
      [ trailBottomLeftChippedOblique
      , trailRightLeftOblique
      , trailTopLeftOblique
      , trailLeftLeftChippedOblique
      , trailChip
      ]
    originX = -thickness / (tanA obliqueAngle)

partRightChippedOblique :: Given Config => Part
partRightChippedOblique = partLeftChippedOblique # reflectX # moveOriginBy (-triangleWidth &| 0)

trailBottomBase :: Given Config => PartTrail
trailBottomBase = origin ~~ (x &| 0)
  where
    x = triangleWidth

trailRightBase :: Given Config => PartTrail
trailRightBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = thickness

trailTopBase :: Given Config => PartTrail
trailTopBase = origin ~~ (x &| 0)
  where
    x = -triangleWidth + thickness / tanA obliqueAngle * 2

trailLeftBase :: Given Config => PartTrail
trailLeftBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = -thickness

partBase :: Given Config => Part
partBase = makePart trails
  where
    trails =
      [ trailBottomBase
      , trailRightBase
      , trailTopBase
      , trailLeftBase
      ]

partChippedBase :: Given Config => Part
partChippedBase = makePart trails # moveOriginBy (originX &| 0)
  where
    trails =
      [ trailTopBase # backward
      , trailChip # reflectX # backward
      , trailChip # rotateHalfTurn
      , trailTopBase
      , trailChip # reflectY # backward
      , trailChip
      ]
    originX = -thickness / (tanA obliqueAngle)

trailBottomLeftAscender :: Given Config => PartTrail
trailBottomLeftAscender = origin ~~ (x &| y)
  where
    x = thickness / (sinA obliqueAngle * 2)
    y = thickness / (cosA obliqueAngle * 2)

trailRightLeftAscender :: Given Config => PartTrail
trailRightLeftAscender = origin ~~ (x &| y)
  where
    x = -y / (tanA obliqueAngle)
    y = descent - overshoot + thickness / (cosA obliqueAngle * 2)

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
    originY = -mean - overshoot + thickness / (cosA obliqueAngle)

partRightAscender :: Given Config => Part
partRightAscender = partLeftAscender # reflectX # moveOriginBy (-triangleWidth &| 0)

trailRightLeftDescender :: Given Config => PartTrail
trailRightLeftDescender = origin ~~ (x &| y)
  where
    x = -y / (tanA obliqueAngle)
    y = descent + thickness / 2

partLeftDescender :: Given Config => Part
partLeftDescender = makePart trails # moveOriginBy (originX &| originY)
  where
    trails = 
      [ trailRightLeftDescender # backward
      , trailBottomLeftAscender
      , trailRightLeftDescender
      , trailBottomLeftAscender # backward
      ]
    originX = -thickness / (tanA obliqueAngle * 2)
    originY = -thickness / 2

partRightDescender :: Given Config => Part
partRightDescender = partLeftDescender # reflectX # moveOriginBy (-triangleWidth &| 0)