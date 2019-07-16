{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Part
  ( partLeftUpOblique
  , partRightUpOblique
  , partUpBase
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.Value


obliqueAngle :: Given Config => Angle Double
obliqueAngle = atan2A (mean + overshoot) (triangleWidth / 2)

trailBottomLeftUpOblique :: Given Config => PartTrail
trailBottomLeftUpOblique = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle

trailRightLeftUpOblique :: Given Config => PartTrail
trailRightLeftUpOblique = origin ~~ (x &| y)
  where
    x = triangleWidth / 2 - thickness / (2 * sinA obliqueAngle)
    y = mean + overshoot - thickness / (2 * cosA obliqueAngle)

trailTopLeftUpOblique :: Given Config => PartTrail
trailTopLeftUpOblique = origin ~~ (x &| y)
  where
    x = -thickness / (2 * sinA obliqueAngle)
    y = thickness / (2 * cosA obliqueAngle)

trailLeftLeftUpOblique :: Given Config => PartTrail
trailLeftLeftUpOblique = origin ~~ (x &| y)
  where
    x = -triangleWidth / 2
    y = -mean - overshoot

partLeftUpOblique :: Given Config => Part
partLeftUpOblique = makePart trails
  where
    trails =
      [ trailBottomLeftUpOblique
      , trailRightLeftUpOblique
      , trailTopLeftUpOblique
      , trailLeftLeftUpOblique
      ]

partRightUpOblique :: Given Config => Part
partRightUpOblique = partLeftUpOblique # reflectX # moveOriginBy (-triangleWidth &| 0)

trailBottomUpBase :: Given Config => PartTrail
trailBottomUpBase = origin ~~ (x &| 0)
  where
    x = triangleWidth

trailRightUpBase :: Given Config => PartTrail
trailRightUpBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = thickness

trailTopUpBase :: Given Config => PartTrail
trailTopUpBase = origin ~~ (x &| 0)
  where
    x = -triangleWidth + thickness / tanA obliqueAngle * 2

trailLeftUpBase :: Given Config => PartTrail
trailLeftUpBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = -thickness

partUpBase :: Given Config => Part
partUpBase = makePart trails
  where
    trails =
      [ trailBottomUpBase
      , trailRightUpBase
      , trailTopUpBase
      , trailLeftUpBase
      ]