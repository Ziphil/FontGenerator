{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Vekos.PartFunc
  ( idealThickness
  , calcTailError
  , searchTailInnerCont
  , calcSpineError
  , searchSpineInnerCont
  )
where

import Data.FontGen
import Data.Ord
import Data.List
import Ziphil.FontGen.Vekos.Config
import Ziphil.FontGen.Vekos.Value


idealThickness :: Given Config => Angle Double -> Double
idealThickness angle =
  if angle >= zero && angle <= quarterTurn
    then coeffX * thicknessX + coeffY * thicknessY
    else 1 / 0
  where
    coeffX = angleRatio angle quarterTurn
    coeffY = 1 - angleRatio angle quarterTurn

calcTailError :: Given Config => Double -> Double -> Double -> Double -> Double
calcTailError bend height innerCont outerCont = abs (distance point base - idealThickness angle / 2)
  where
    angle = angleBetween (point .-. base) unitX ^-^ quarterTurn
    point = head $ closestPoint segment base
    base = (-bend / 2 + thicknessX / 2 &| -height / 2)
    segment = origin ~> (0 &| -innerCont) ~~ (0 &| outerCont) <~ (-bend &| -height)

searchTailInnerCont :: Given Config => Double -> Double -> Double -> Double
searchTailInnerCont bend height outerCont = minimumBy (comparing calcTailError') list
  where
    calcTailError' innerCont = calcTailError bend height innerCont outerCont
    list = [0, interval .. height]
    interval = 0.5

calcSpineError :: Given Config => Double -> Double -> Double -> Double -> Double
calcSpineError bend width innerCont outerCont = abs (distance point base - idealThickness angle / 2)
  where
    angle = angleBetween (point .-. base) unitX ^-^ quarterTurn
    point = head $ closestPoint segment base
    base = (width / 2 &| bend / 2 - thicknessY / 2)
    segment = origin ~> (innerCont &| 0) ~~ (-outerCont &| 0) <~ (width &| bend)

searchSpineInnerCont :: Given Config => Double -> Double -> Double -> Double
searchSpineInnerCont bend width outerCont = minimumBy (comparing calcSpineError') list
  where
    calcSpineError' innerCont = calcSpineError bend width innerCont outerCont
    list = [0, interval .. width]
    interval = 0.5