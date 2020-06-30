{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Gilit.Part
  ( partLeftOblique
  , partRightOblique
  , partLeftShortOblique
  , partRightShortOblique
  , partLeftChippedOblique
  , partRightChippedOblique
  , partLeftCutOblique
  , partRightCutOblique
  , partLeftCenterOblique
  , partRightCenterOblique
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
  , partDiamond
  , partTransphone
  , partUpperAcute
  , partUpperGrave
  , partUpperCircumflex
  , partLowerAcute
  , partLowerGrave
  , partLowerCircumflex
  , partDot
  , obliqueAngle
  , horizontalTransphoneGap
  , defaultLeftEnd
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

boneCut :: Given Config => Bone
boneCut = origin ~~ (x &| y)
  where
    x = thickness / (sinA obliqueAngle * 2)
    y = thickness / (cosA obliqueAngle * 2)

boneHorizontalCut :: Given Config => Bone
boneHorizontalCut = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle

boneRightLeftOblique :: Given Config => Bone
boneRightLeftOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = triangleHeight + thickness / 2

boneLeftLeftOblique :: Given Config => Bone
boneLeftLeftOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight - thickness / (cosA obliqueAngle * 2) - thickness / 2

partLeftOblique :: Given Config => Part
partLeftOblique = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneHorizontalCut
      boneRightLeftOblique
      boneCut # reflectY # backward
      boneLeftLeftOblique
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightOblique :: Given Config => Part
partRightOblique = partLeftOblique # reflectX'

boneRightLeftShortOblique :: Given Config => Bone
boneRightLeftShortOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = triangleHeight - diamondGap * sinA obliqueAngle + thickness / 2 - thickness / (cosA obliqueAngle * 2)

boneLeftLeftShortOblique :: Given Config => Bone
boneLeftLeftShortOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight + diamondGap * sinA obliqueAngle - thickness / 2

partLeftShortOblique :: Given Config => Part
partLeftShortOblique = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneHorizontalCut
      boneRightLeftShortOblique
      boneCut # reflectY # backward
      boneLeftLeftShortOblique
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightShortOblique :: Given Config => Part
partRightShortOblique = partLeftShortOblique # reflectX'

boneHorizontalChippedCut :: Given Config => Bone
boneHorizontalChippedCut = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle - thickness / tanA obliqueAngle

boneLeftLeftChippedOblique :: Given Config => Bone
boneLeftLeftChippedOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight - thickness / (cosA obliqueAngle * 2)

boneChip :: Given Config => Bone
boneChip = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -thickness / 2

partLeftChippedOblique :: Given Config => Part
partLeftChippedOblique = partBy bones #.~> (originX &| 0) 
  where
    bones = do
      boneChip
      boneHorizontalChippedCut
      boneRightLeftOblique
      boneCut # reflectY # backward
      boneLeftLeftChippedOblique
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedOblique :: Given Config => Part
partRightChippedOblique = partLeftChippedOblique # reflectX'

boneLeftLeftCutOblique :: Given Config => Bone
boneLeftLeftCutOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight - thickness

partLeftCutOblique :: Given Config => Part
partLeftCutOblique = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneHorizontalCut
      boneRightLeftOblique
      boneChip # rotateHalfTurn
      boneHorizontalChippedCut # backward
      boneLeftLeftCutOblique
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightCutOblique :: Given Config => Part
partRightCutOblique = partLeftCutOblique # reflectX'

partLeftCenterOblique :: Given Config => Part
partLeftCenterOblique = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneChip
      boneHorizontalChippedCut
      boneRightLeftOblique
      boneChip # rotateHalfTurn
      boneHorizontalChippedCut # backward
      boneRightLeftOblique # backward
    originX = thickness / (sinA obliqueAngle * 2)

partRightCenterOblique :: Given Config => Part
partRightCenterOblique = partLeftCenterOblique # reflectX'

boneBottomBase :: Given Config => Bone
boneBottomBase = origin ~~ (x &| 0)
  where
    x = triangleWidth + thickness / sinA obliqueAngle + thickness / tanA obliqueAngle

boneRightBase :: Given Config => Bone
boneRightBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = thickness

boneTopBase :: Given Config => Bone
boneTopBase = origin ~~ (x &| 0)
  where
    x = -triangleWidth - thickness / sinA obliqueAngle + thickness / tanA obliqueAngle

boneLeftBase :: Given Config => Bone
boneLeftBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = -thickness

partBase :: Given Config => Part
partBase = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneBottomBase
      boneRightBase
      boneTopBase
      boneLeftBase
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

boneBottomLeftChippedBase :: Given Config => Bone
boneBottomLeftChippedBase = origin ~~ (x &| 0)
  where
    x = triangleWidth + thickness / sinA obliqueAngle

partLeftChippedBase :: Given Config => Part
partLeftChippedBase = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneChip
      boneBottomLeftChippedBase
      boneRightBase
      boneTopBase
      boneChip # reflectY # backward
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedBase :: Given Config => Part
partRightChippedBase = partLeftChippedBase # reflectX'

partChippedBase :: Given Config => Part
partChippedBase = partBy bones #.~> (originX &| 0)
  where
    bones = do
      boneChip
      boneTopBase # backward
      boneChip # reflectX # backward
      boneChip # rotateHalfTurn
      boneTopBase
      boneChip # reflectY # backward
    originX = thickness / (sinA obliqueAngle * 2)

boneRightLeftAscender :: Given Config => Bone
boneRightLeftAscender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = ascenderHeight + thickness / (cosA obliqueAngle * 4)

partLeftAscender :: Given Config => Part
partLeftAscender = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneCut
      boneRightLeftAscender
      boneCut # backward
      boneRightLeftAscender # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight + thickness / (cosA obliqueAngle * 2)

partRightAscender :: Given Config => Part
partRightAscender = partLeftAscender # reflectX'

boneLeftLeftDescender :: Given Config => Bone
boneLeftLeftDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -ascenderHeight - thickness / 2 - thickness / (cosA obliqueAngle * 4)

boneRightLeftDescender :: Given Config => Bone
boneRightLeftDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = ascenderHeight + thickness / 2 - thickness / (cosA obliqueAngle * 4)

partLeftDescender :: Given Config => Part
partLeftDescender = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneLeftLeftDescender
      boneCut
      boneRightLeftDescender
      boneHorizontalCut # backward
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = -thickness / 2

partRightDescender :: Given Config => Part
partRightDescender = partLeftDescender # reflectX'

boneLeftLeftChippedDescender :: Given Config => Bone
boneLeftLeftChippedDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -ascenderHeight - thickness / (cosA obliqueAngle * 4)

partLeftChippedDescender :: Given Config => Part
partLeftChippedDescender = partBy bones #.~> (originX &| 0)
  where
    bones = do 
      boneLeftLeftChippedDescender
      boneCut
      boneRightLeftDescender
      boneHorizontalChippedCut # backward
      boneChip
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedDescender :: Given Config => Part
partRightChippedDescender = partLeftChippedDescender # reflectX'

partDiamond :: Given Config => Part
partDiamond = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneCut
      boneCut # reflectY # backward
      boneCut # rotateHalfTurn
      boneCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight + thickness / (cosA obliqueAngle * 2)

horizontalTransphoneGap :: Given Config => Double
horizontalTransphoneGap = transphoneGap / sinA obliqueAngle

boneRightTransphone :: Given Config => Bone
boneRightTransphone = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = triangleHeight + thickness

partTransphone :: Given Config => Part
partTransphone = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneHorizontalCut
      boneRightTransphone
      boneHorizontalCut # backward
      boneRightTransphone # backward
    originX = -triangleWidth - horizontalTransphoneGap - thickness / (sinA obliqueAngle * 2) - thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

overshoot :: Given Config => Double
overshoot = thickness / (cosA obliqueAngle * 2) - thickness / 2

boneUpperAcuteCut :: Given Config => Bone
boneUpperAcuteCut = origin ~~ (x &| y)
  where
    x = thickness * acuteRatio / (sinA obliqueAngle * 2)
    y = thickness * acuteRatio / (cosA obliqueAngle * 2)

boneUpperAcuteHorizontalCut :: Given Config => Bone
boneUpperAcuteHorizontalCut = origin ~~ (x &| 0)
  where
    x = thickness * acuteRatio / sinA obliqueAngle

partUpperAcute :: Given Config => Part
partUpperAcute = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneUpperAcuteCut # backward
      boneUpperAcuteHorizontalCut
      boneUpperAcuteCut # reflectX
    originX = -triangleWidth / 2
    originY = -triangleHeight - diacriticGap - thickness / (cosA obliqueAngle * 2) - thickness * acuteRatio / (cosA obliqueAngle * 2)

partUpperGrave :: Given Config => Part
partUpperGrave = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneUpperAcuteCut
      boneUpperAcuteHorizontalCut # backward
      boneUpperAcuteCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight - diacriticGap - thickness / (cosA obliqueAngle * 2) + overshoot * diacriticOvershootRatio

partUpperCircumflex :: Given Config => Part
partUpperCircumflex = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneCut
      boneCut # reflectY # backward
      boneCut # rotateHalfTurn
      boneCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight - diacriticGap - thickness / (cosA obliqueAngle * 2) + overshoot * diacriticOvershootRatio

partLowerAcute :: Given Config => Part
partLowerAcute = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneUpperAcuteCut
      boneUpperAcuteHorizontalCut # backward
      boneUpperAcuteCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = diacriticGap + thickness / 2 + thickness * acuteRatio / (cosA obliqueAngle * 2)

partLowerGrave :: Given Config => Part
partLowerGrave = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneUpperAcuteCut # backward
      boneUpperAcuteHorizontalCut
      boneUpperAcuteCut # reflectX
    originX = -triangleWidth / 2
    originY = diacriticGap + thickness / 2 - overshoot * diacriticOvershootRatio

partLowerCircumflex :: Given Config => Part
partLowerCircumflex = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneCut
      boneCut # reflectY # backward
      boneCut # rotateHalfTurn
      boneCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = diacriticGap + thickness / 2 + thickness / cosA obliqueAngle - overshoot * diacriticOvershootRatio

partDot :: Given Config => Part
partDot = partBy bones #.~> (originX &| originY)
  where
    bones = do
      boneCut
      boneCut # reflectY # backward
      boneCut # rotateHalfTurn
      boneCut # reflectX # backward
    originX = -triangleWidth / 4
    originY = -triangleHeight / 2 + thickness / (cosA obliqueAngle * 2)

defaultLeftEnd :: Given Config => Double
defaultLeftEnd = triangleWidth / 4 - gap / (sinA obliqueAngle * 2) - thickness / (sinA obliqueAngle * 2)

widthDifference :: Given Config => Double
widthDifference = triangleWidth / 2 - gap / sinA obliqueAngle - thickness / sinA obliqueAngle

ascent :: Given Config => Double
ascent = triangleHeight + ascenderHeight + maxThickness / (cosA maxObliqueAngle * 4)

descent :: Given Config => Double
descent = ascenderHeight + maxThickness / (cosA maxObliqueAngle * 4)

em :: Given Config => Double
em = ascent + descent

actualAscent :: Given Config => Double
actualAscent = ascent + extraAscent

actualDescent :: Given Config => Double
actualDescent = descent + extraDescent

actualEm :: Given Config => Double
actualEm = actualDescent + actualAscent