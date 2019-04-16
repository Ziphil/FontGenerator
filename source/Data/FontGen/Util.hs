{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.FontGen.Util
  ( (&|)
  , (@|)
  , rotateHalfTurn
  , rotateQuarterTurn
  , straight'
  , bezier3'
  , bezier2'
  )
where

import Diagrams.Prelude


-- 直交座標系で表されたデータからベクトルや点を生成します。
infixl 1 &|
(&|) :: Coordinates c => PrevDim c -> FinalCoord c -> c
prev &| final = pr prev final

class PolarCoordinates c where
  type PrevPolarDim c
  type FinalTheta c
  polar :: PrevPolarDim c -> FinalTheta c -> c

instance Floating n => PolarCoordinates (V2 n) where
  type PrevPolarDim (V2 n) = n
  type FinalTheta (V2 n) = n
  polar dist theta = angleV (theta @@ deg) ^* dist

instance PolarCoordinates (v n) => PolarCoordinates (Point v n) where
  type PrevPolarDim (Point v n) = PrevPolarDim (v n)
  type FinalTheta (Point v n) = FinalTheta (v n)
  polar prev theta = P (polar prev theta)

-- 極座標系で表されたデータからベクトルや点を生成します。
infixl 1 @|
(@|) :: PolarCoordinates c => PrevPolarDim c -> FinalTheta c -> c
prev @| theta = polar prev theta

-- 与えられた図形を 180° 回転します。
rotateHalfTurn :: (InSpace V2 n t, Floating n, Transformable t) => t -> t
rotateHalfTurn = rotate halfTurn

-- 与えられた図形を 90° 回転します。
rotateQuarterTurn :: (InSpace V2 n t, Floating n, Transformable t) => t -> t
rotateQuarterTurn = rotate quarterTurn

class (Metric (V s), OrderedField (N s)) => SegmentLike s where
  segmentLike :: Segment Closed (V s) (N s) -> s

instance (Metric v, OrderedField n) => SegmentLike (Segment Closed v n) where
  segmentLike = id

instance TrailLike t => SegmentLike t where
  segmentLike = fromSegments . (: [])

straight' :: SegmentLike s => Vn s -> s
straight' = segmentLike . straight

bezier3' :: SegmentLike s => Vn s -> Vn s -> Vn s -> s
bezier3' = ((segmentLike .) .) . bezier3

bezier2' :: SegmentLike s => Vn s -> Vn s -> s
bezier2' cont end = bezier3' fstCont sndCont end
  where
    fstCont = cont ^* (2 / 3)
    sndCont = cont ^* (2 / 3) ^+^ end ^* (1 / 3)