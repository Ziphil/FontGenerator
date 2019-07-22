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
  , partTransphone
  , partDiamond
  , partDot
  , obliqueAngle
  , horizontalTransphoneGap
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
partLeftOblique = makePart rims # moveOriginBy (originX &| originY)
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
partLeftShortOblique = makePart rims # moveOriginBy (originX &| originY)
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
partLeftChippedOblique = makePart rims # moveOriginBy (originX &| 0) 
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
partLeftCutOblique = makePart rims # moveOriginBy (originX &| originY)
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
partLeftCenterOblique = makePart rims # moveOriginBy (originX &| 0)
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
partBase = makePart rims # moveOriginBy (originX &| originY)
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
partLeftChippedBase = makePart rims # moveOriginBy (originX &| 0)
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
partChippedBase = makePart rims # moveOriginBy (originX &| 0)
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
partLeftAscender = makePart rims # moveOriginBy (originX &| originY)
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
partLeftDescender = makePart rims # moveOriginBy (originX &| originY)
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
partLeftChippedDescender = makePart rims # moveOriginBy (originX &| 0)
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

horizontalTransphoneGap :: Given Config => Double
horizontalTransphoneGap = transphoneGap / sinA obliqueAngle

rimRightTransphone :: Given Config => Rim
rimRightTransphone = origin ~~ (x &| y)
  where
    x = -y / tanA obliqueAngle
    y = triangleHeight + thickness

partTransphone :: Given Config => Part
partTransphone = makePart rims # moveOriginBy (originX &| originY)
  where
    rims = do
      rimHorizontalCut
      rimRightTransphone
      rimHorizontalCut # backward
      rimRightTransphone # backward
    originX = -triangleWidth - horizontalTransphoneGap - thickness / (sinA obliqueAngle * 2) - thickness / (tanA obliqueAngle * 2)
    originY = thickness / 2

partDiamond :: Given Config => Part
partDiamond = makePart rims # moveOriginBy (originX &| originY)
  where
    rims = do
      rimCut
      rimCut # reflectY # backward
      rimCut # rotateHalfTurn
      rimCut # reflectX # backward
    originX = -triangleWidth / 2
    originY = -triangleHeight + thickness / (cosA obliqueAngle * 2)

partDot :: Given Config => Part
partDot = makePart rims # moveOriginBy (originX &| originY)
  where
    rims = do
      rimCut
      rimCut # reflectY # backward
      rimCut # rotateHalfTurn
      rimCut # reflectX # backward
    originX = -triangleWidth / 4
    originY = -triangleHeight / 2 + thickness / (cosA obliqueAngle * 2)

defaultLeftX :: Given Config => Double
defaultLeftX = triangleWidth / 4 - gap / (sinA obliqueAngle * 2) - thickness / (sinA obliqueAngle * 2)

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