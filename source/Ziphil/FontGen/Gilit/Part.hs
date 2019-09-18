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

rimCut :: Given Config => Rim
rimCut = origin ~~ (x &| y)
  where
    x = thickness / (sinA obliqueAngle * 2)
    y = thickness / (cosA obliqueAngle * 2)

rimHorizontalCut :: Given Config => Rim
rimHorizontalCut = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle

rimRightLeftOblique :: Given Config => Rim
rimRightLeftOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = triangleHeight + thickness / 2

rimLeftLeftOblique :: Given Config => Rim
rimLeftLeftOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight - thickness / (cosA obliqueAngle * 2) - thickness / 2

partLeftOblique :: Given Config => Part
partLeftOblique = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimHorizontalCut
      rimRightLeftOblique
      rimCut # reflectY # backward
      rimLeftLeftOblique
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightOblique :: Given Config => Part
partRightOblique = partLeftOblique # reflectX'

rimRightLeftShortOblique :: Given Config => Rim
rimRightLeftShortOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = triangleHeight - diamondGap * sinA obliqueAngle + thickness / 2 - thickness / (cosA obliqueAngle * 2)

rimLeftLeftShortOblique :: Given Config => Rim
rimLeftLeftShortOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight + diamondGap * sinA obliqueAngle - thickness / 2

partLeftShortOblique :: Given Config => Part
partLeftShortOblique = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimHorizontalCut
      rimRightLeftShortOblique
      rimCut # reflectY # backward
      rimLeftLeftShortOblique
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightShortOblique :: Given Config => Part
partRightShortOblique = partLeftShortOblique # reflectX'

rimHorizontalChippedCut :: Given Config => Rim
rimHorizontalChippedCut = origin ~~ (x &| 0)
  where
    x = thickness / sinA obliqueAngle - thickness / tanA obliqueAngle

rimLeftLeftChippedOblique :: Given Config => Rim
rimLeftLeftChippedOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight - thickness / (cosA obliqueAngle * 2)

rimChip :: Given Config => Rim
rimChip = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -thickness / 2

partLeftChippedOblique :: Given Config => Part
partLeftChippedOblique = partBy rims #.~> (originX &| 0) 
  where
    rims = do
      rimChip
      rimHorizontalChippedCut
      rimRightLeftOblique
      rimCut # reflectY # backward
      rimLeftLeftChippedOblique
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedOblique :: Given Config => Part
partRightChippedOblique = partLeftChippedOblique # reflectX'

rimLeftLeftCutOblique :: Given Config => Rim
rimLeftLeftCutOblique = origin ~~ (x &| y)
  where
    x = y / tanA obliqueAngle
    y = -triangleHeight - thickness

partLeftCutOblique :: Given Config => Part
partLeftCutOblique = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimHorizontalCut
      rimRightLeftOblique
      rimChip # rotateHalfTurn
      rimHorizontalChippedCut # backward
      rimLeftLeftCutOblique
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partRightCutOblique :: Given Config => Part
partRightCutOblique = partLeftCutOblique # reflectX'

partLeftCenterOblique :: Given Config => Part
partLeftCenterOblique = partBy rims #.~> (originX &| 0)
  where
    rims = do
      rimChip
      rimHorizontalChippedCut
      rimRightLeftOblique
      rimChip # rotateHalfTurn
      rimHorizontalChippedCut # backward
      rimRightLeftOblique # backward
    originX = thickness / (sinA obliqueAngle * 2)

partRightCenterOblique :: Given Config => Part
partRightCenterOblique = partLeftCenterOblique # reflectX'

rimBottomBase :: Given Config => Rim
rimBottomBase = origin ~~ (x &| 0)
  where
    x = triangleWidth + thickness / sinA obliqueAngle + thickness / tanA obliqueAngle

rimRightBase :: Given Config => Rim
rimRightBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = thickness

rimTopBase :: Given Config => Rim
rimTopBase = origin ~~ (x &| 0)
  where
    x = -triangleWidth - thickness / sinA obliqueAngle + thickness / tanA obliqueAngle

rimLeftBase :: Given Config => Rim
rimLeftBase = origin ~~ (x &| y)
  where
    x = -thickness / tanA obliqueAngle
    y = -thickness

partBase :: Given Config => Part
partBase = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimBottomBase
      rimRightBase
      rimTopBase
      rimLeftBase
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

rimBottomLeftChippedBase :: Given Config => Rim
rimBottomLeftChippedBase = origin ~~ (x &| 0)
  where
    x = triangleWidth + thickness / sinA obliqueAngle

partLeftChippedBase :: Given Config => Part
partLeftChippedBase = partBy rims #.~> (originX &| 0)
  where
    rims = do
      rimChip
      rimBottomLeftChippedBase
      rimRightBase
      rimTopBase
      rimChip # reflectY # backward
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedBase :: Given Config => Part
partRightChippedBase = partLeftChippedBase # reflectX'

partChippedBase :: Given Config => Part
partChippedBase = partBy rims #.~> (originX &| 0)
  where
    rims = do
      rimChip
      rimTopBase # backward
      rimChip # reflectX # backward
      rimChip # rotateHalfTurn
      rimTopBase
      rimChip # reflectY # backward
    originX = thickness / (sinA obliqueAngle * 2)

rimRightLeftAscender :: Given Config => Rim
rimRightLeftAscender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = ascenderHeight + thickness / (cosA obliqueAngle * 4)

partLeftAscender :: Given Config => Part
partLeftAscender = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimCut
      rimRightLeftAscender
      rimCut # backward
      rimRightLeftAscender # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight + thickness / (cosA obliqueAngle * 2)

partRightAscender :: Given Config => Part
partRightAscender = partLeftAscender # reflectX'

rimLeftLeftDescender :: Given Config => Rim
rimLeftLeftDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -ascenderHeight - thickness / 2 - thickness / (cosA obliqueAngle * 4)

rimRightLeftDescender :: Given Config => Rim
rimRightLeftDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = ascenderHeight + thickness / 2 - thickness / (cosA obliqueAngle * 4)

partLeftDescender :: Given Config => Part
partLeftDescender = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimLeftLeftDescender
      rimCut
      rimRightLeftDescender
      rimHorizontalCut # backward
    originX = thickness / (sinA obliqueAngle * 2) + thickness / (tanA obliqueAngle * 2)
    originY = -thickness / 2

partRightDescender :: Given Config => Part
partRightDescender = partLeftDescender # reflectX'

rimLeftLeftChippedDescender :: Given Config => Rim
rimLeftLeftChippedDescender = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = -ascenderHeight - thickness / (cosA obliqueAngle * 4)

partLeftChippedDescender :: Given Config => Part
partLeftChippedDescender = partBy rims #.~> (originX &| 0)
  where
    rims = do 
      rimLeftLeftChippedDescender
      rimCut
      rimRightLeftDescender
      rimHorizontalChippedCut # backward
      rimChip
    originX = thickness / (sinA obliqueAngle * 2)

partRightChippedDescender :: Given Config => Part
partRightChippedDescender = partLeftChippedDescender # reflectX'

partDiamond :: Given Config => Part
partDiamond = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimCut
      rimCut # reflectY # backward
      rimCut # rotateHalfTurn
      rimCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight + thickness / (cosA obliqueAngle * 2)

horizontalTransphoneGap :: Given Config => Double
horizontalTransphoneGap = transphoneGap / sinA obliqueAngle

rimRightTransphone :: Given Config => Rim
rimRightTransphone = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = triangleHeight + thickness

partTransphone :: Given Config => Part
partTransphone = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimHorizontalCut
      rimRightTransphone
      rimHorizontalCut # backward
      rimRightTransphone # backward
    originX = -triangleWidth - horizontalTransphoneGap - thickness / (sinA obliqueAngle * 2) - thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

overshoot :: Given Config => Double
overshoot = thickness / (cosA obliqueAngle * 2) - thickness / 2

rimUpperAcuteCut :: Given Config => Rim
rimUpperAcuteCut = origin ~~ (x &| y)
  where
    x = thickness * acuteRatio / (sinA obliqueAngle * 2)
    y = thickness * acuteRatio / (cosA obliqueAngle * 2)

rimUpperAcuteHorizontalCut :: Given Config => Rim
rimUpperAcuteHorizontalCut = origin ~~ (x &| 0)
  where
    x = thickness * acuteRatio / sinA obliqueAngle

partUpperAcute :: Given Config => Part
partUpperAcute = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimUpperAcuteCut # backward
      rimUpperAcuteHorizontalCut
      rimUpperAcuteCut # reflectX
    originX = -triangleWidth / 2
    originY = -triangleHeight - diacriticGap - thickness / (cosA obliqueAngle * 2) - thickness * acuteRatio / (cosA obliqueAngle * 2)

partUpperGrave :: Given Config => Part
partUpperGrave = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimUpperAcuteCut
      rimUpperAcuteHorizontalCut # backward
      rimUpperAcuteCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight - diacriticGap - thickness / (cosA obliqueAngle * 2) + overshoot * diacriticOvershootRatio

partUpperCircumflex :: Given Config => Part
partUpperCircumflex = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimCut
      rimCut # reflectY # backward
      rimCut # rotateHalfTurn
      rimCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight - diacriticGap - thickness / (cosA obliqueAngle * 2) + overshoot * diacriticOvershootRatio

partLowerAcute :: Given Config => Part
partLowerAcute = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimUpperAcuteCut
      rimUpperAcuteHorizontalCut # backward
      rimUpperAcuteCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = diacriticGap + thickness / 2 + thickness * acuteRatio / (cosA obliqueAngle * 2)

partLowerGrave :: Given Config => Part
partLowerGrave = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimUpperAcuteCut # backward
      rimUpperAcuteHorizontalCut
      rimUpperAcuteCut # reflectX
    originX = -triangleWidth / 2
    originY = diacriticGap + thickness / 2 - overshoot * diacriticOvershootRatio

partLowerCircumflex :: Given Config => Part
partLowerCircumflex = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimCut
      rimCut # reflectY # backward
      rimCut # rotateHalfTurn
      rimCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = diacriticGap + thickness / 2 + thickness / cosA obliqueAngle - overshoot * diacriticOvershootRatio

partDot :: Given Config => Part
partDot = partBy rims #.~> (originX &| originY)
  where
    rims = do
      rimCut
      rimCut # reflectY # backward
      rimCut # rotateHalfTurn
      rimCut # reflectX # backward
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