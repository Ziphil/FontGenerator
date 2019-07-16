{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Part
  ( partLeftOblique
  , partRightOblique
  , partBase
  , partLeftAscender
  , partRightAscender
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