{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Data.FontGen.Util
  ( (&|)
  , (@|)
  , rotateHalfTurn
  , rotateQuarterTurn
  , (~>)
  , (<~)
  , (~:~)
  , (~~)
  )
where

import Control.Applicative
import Diagrams.Prelude hiding ((<~), (~~))


-- 直交座標系で表されたデータからベクトルや点を生成します。
infixl 4 &|
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
infixl 4 @|
(@|) :: PolarCoordinates c => PrevPolarDim c -> FinalTheta c -> c
prev @| theta = polar prev theta

-- 与えられた図形を 180° 回転します。
rotateHalfTurn :: (InSpace V2 n t, Floating n, Transformable t) => t -> t
rotateHalfTurn = rotate halfTurn

-- 与えられた図形を 90° 回転します。
rotateQuarterTurn :: (InSpace V2 n t, Floating n, Transformable t) => t -> t
rotateQuarterTurn = rotate quarterTurn

data EndPoint s = EndPoint (Point (V s) (N s)) (V s (N s))

-- 3 次ベジエ曲線の始点側の端点と制御点を生成します。
infix 1 ~>
(~>) :: (V s ~ v, N s ~ n) => Point v n -> v n -> EndPoint s
point ~> cont = EndPoint point cont

-- 3 次ベジエ曲線の終点側の端点と制御点を生成します。
infix 1 <~
(<~) :: (V s ~ v, N s ~ n) => v n -> Point v n -> EndPoint s
cont <~ point = EndPoint point cont

class (Metric (V s), OrderedField (N s)) => SegmentLike s where
  segmentLike :: Located (Segment Closed (V s) (N s)) -> s

instance (Metric v, OrderedField n) => SegmentLike (Segment Closed v n) where
  segmentLike = unLoc

instance (Metric v, OrderedField n) => SegmentLike (FixedSegment v n) where
  segmentLike = mkFixedSeg

instance (Metric v, OrderedField n) => SegmentLike (Trail v n) where
  segmentLike = trailLike . (mapLoc $ fromSegments . (: []))

instance SegmentLike t => SegmentLike (Located t) where
  segmentLike = liftA2 at segmentLike loc

-- 始点側と終点側それぞれの端点と制御点の情報から、3 次ベジエ曲線を生成します。
-- 生成される値が原点をもつ場合、その原点は始点に設定されます。
infix 0 ~:~
(~:~) :: (InSpace v n s, SegmentLike s) => EndPoint s -> EndPoint s -> s
(EndPoint initPoint initCont) ~:~ (EndPoint termPoint termCont) = segmentLike $ at segment initPoint
  where
    segment = bezier3 fstCont sndCont termVec
    fstCont = initCont
    sndCont = termVec ^+^ termCont
    termVec = termPoint .-. initPoint

infix 0 ~~
(~~) :: (InSpace v n s, SegmentLike s) => Point v n -> Point v n -> s
initPoint ~~ termPoint = segmentLike $ at segment initPoint
  where
    segment = straight termVec
    termVec = termPoint .-. initPoint