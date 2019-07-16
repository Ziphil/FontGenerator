{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Part
  ( partLeftOblique
  , partRightOblique
  , partBase
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
    x = triangleWidth / 2 - thickness / (2 * sinA obliqueAngle)
    y = mean + overshoot - thickness / (2 * cosA obliqueAngle)

trailTopLeftOblique :: Given Config => PartTrail
trailTopLeftOblique = origin ~~ (x &| y)
  where
    x = -thickness / (2 * sinA obliqueAngle)
    y = thickness / (2 * cosA obliqueAngle)

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