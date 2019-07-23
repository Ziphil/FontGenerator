{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Data.FontGen.Util
  ( rotateHalfTurn
  , rotateQuarterTurn
  , OrthoCoordinates (..)
  , (&|)
  , PolarCoordinates (..)
  , (@|)
  , Backwardable (..)
  , EndPoint
  , PointLike (..)
  , (~>)
  , (<~)
  , origin
  , SegmentLike (..)
  , (~~)
  , (&~)
  , skip
  , (@~)
  , (@=)
  )
where

import qualified Control.Lens as Lens
import Control.Applicative
import Control.Monad.State
import Data.FontGen.MonoidState
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Diagrams.Prelude hiding ((<~), (~~), (&~), origin)


-- 与えられた図形を 180° 回転します。
rotateHalfTurn :: (InSpace V2 n t, Floating n, Transformable t) => t -> t
rotateHalfTurn = rotate halfTurn

-- 与えられた図形を 90° 回転します。
rotateQuarterTurn :: (InSpace V2 n t, Floating n, Transformable t) => t -> t
rotateQuarterTurn = rotate quarterTurn

class OrthoCoordinates c where
  type PrevOrthoDim c
  ortho :: PrevOrthoDim c -> N c -> c

instance OrthoCoordinates (V2 n) where
  type PrevOrthoDim (V2 n) = n
  ortho x y = V2 x y

instance (N (v n) ~ n, OrthoCoordinates (v n)) => OrthoCoordinates (Point v n) where
  type PrevOrthoDim (Point v n) = PrevOrthoDim (v n)
  ortho prev val = P (ortho prev val)

instance (N (v n) ~ n, OrthoCoordinates (v n)) => OrthoCoordinates (EndPoint v n) where
  type PrevOrthoDim (EndPoint v n) = PrevOrthoDim (v n)
  ortho prev val = ortho prev val :~> Nothing

-- 直交座標系で表されたデータからベクトルや点を生成します。
infixl 4 &|
(&|) :: OrthoCoordinates c => PrevOrthoDim c -> N c -> c
prev &| val = ortho prev val

class PolarCoordinates c where
  type PrevPolarDim c
  polar :: PrevPolarDim c -> Angle (N c) -> c

instance Floating n => PolarCoordinates (V2 n) where
  type PrevPolarDim (V2 n) = n
  polar dist angle = angleV angle ^* dist

instance (N (v n) ~ n, PolarCoordinates (v n)) => PolarCoordinates (Point v n) where
  type PrevPolarDim (Point v n) = PrevPolarDim (v n)
  polar prev angle = P (polar prev angle)

instance (N (v n) ~ n, PolarCoordinates (v n)) => PolarCoordinates (EndPoint v n) where
  type PrevPolarDim (EndPoint v n) = PrevPolarDim (v n)
  polar prev angle = polar prev angle :~> Nothing

-- 極座標系で表されたデータからベクトルや点を生成します。
infixl 4 @|
(@|) :: PolarCoordinates c => PrevPolarDim c -> Angle (N c) -> c
prev @| angle = polar prev angle

class Backwardable a where
  backward :: a -> a

instance (Num n, Additive v) => Backwardable (Segment Closed v n) where
  backward = reverseSegment

instance (Metric v, OrderedField n) => Backwardable (Trail v n) where
  backward = reverseTrail

instance (Metric v, OrderedField n) => Backwardable (Path v n) where
  backward = reversePath

instance Backwardable a => Backwardable [a] where
  backward = map backward . reverse

data EndPoint v n = Point v n :~> Maybe (v n)

type instance V (EndPoint v n) = v
type instance N (EndPoint v n) = n

class PointLike p where
  pointLike :: Point (V p) (N p) -> p

instance PointLike (Point v n) where
  pointLike = id

instance PointLike (EndPoint v n) where
  pointLike point = point :~> Nothing

-- 3 次ベジエ曲線の始点側の端点と制御点を生成します。
infix 2 ~>
(~>) :: Point v n -> v n -> EndPoint v n
point ~> cont = point :~> Just cont

-- 3 次ベジエ曲線の終点側の端点と制御点を生成します。
infix 2 <~
(<~) :: v n -> Point v n -> EndPoint v n
cont <~ point = point :~> Just cont

-- 原点を表します。
-- ベジエ曲線の端点と制御点としても利用できるように、型を多相化してあります。
origin :: (InSpace v n p, PointLike p) => p
origin = pointLike $ P zero

class (Metric (V s), OrderedField (N s)) => SegmentLike s where
  segmentLike :: Located (Segment Closed (V s) (N s)) -> s

instance (Metric v, OrderedField n) => SegmentLike (Segment Closed v n) where
  segmentLike = unLoc

instance (Metric v, OrderedField n) => SegmentLike (FixedSegment v n) where
  segmentLike = mkFixedSeg

instance (Metric v, OrderedField n) => SegmentLike (Trail v n) where
  segmentLike = trailLike . (mapLoc $ fromSegments . (: []))

instance (Metric v, OrderedField n) => SegmentLike (Path v n) where
  segmentLike = trailLike . (mapLoc $ fromSegments . (: []))

instance SegmentLike t => SegmentLike (Located t) where
  segmentLike = liftA2 at segmentLike loc

makeStraight :: (InSpace v n s, SegmentLike s) => Point v n -> Point v n -> s
makeStraight initPoint termPoint = segmentLike $ at segment initPoint
  where
    segment = straight termVec
    termVec = termPoint .-. initPoint

makeBezier3 :: (InSpace v n s, SegmentLike s) => Point v n -> v n -> Point v n -> v n -> s
makeBezier3 initPoint initCont termPoint termCont = segmentLike $ at segment initPoint
  where
    segment = bezier3 fstCont sndCont termVec
    fstCont = initCont
    sndCont = termVec ^+^ termCont
    termVec = termPoint .-. initPoint

-- 始点側と終点側それぞれの端点と制御点の情報から、3 次ベジエ曲線を生成します。
-- 始点と終点がともに制御点をもたない場合は、単に始点と終点を結ぶ直線を生成します。
-- 生成される値が原点をもつ場合、その原点は始点に設定されます。
infix 1 ~~
(~~) :: (InSpace v n s, SegmentLike s) => EndPoint v n -> EndPoint v n -> s
initPoint :~> initCont ~~ termPoint :~> termCont =
  if isNothing initCont && isNothing termCont
    then makeStraight initPoint termPoint
    else makeBezier3 initPoint (fromMaybe zero initCont) termPoint (fromMaybe zero termCont)

-- レンズを用いた操作を行うためのユーティリティ演算子です。
-- 代入系の演算子と一緒に用いられるよう、lens パッケージで定義されているものより優先度が高く設定してあります。
infixl 8 &~
(&~) :: s -> State s a -> s
val &~ state = execState state val

skip :: Monad m => m ()
skip = return ()

type instance V (MonoidState s a) = V s
type instance N (MonoidState s a) = N s

instance (Monoid t, HasOrigin t) => HasOrigin (MonoidState t ()) where
  moveOriginTo point state = add (moveOriginTo point $ execMonoidState' state)

instance (Monoid t, Transformable t) => Transformable (MonoidState t ()) where
  transform op state = add (transform op $ execMonoidState' state)

instance (Monoid t, Backwardable t) => Backwardable (MonoidState t ()) where
  backward state = add (backward $ execMonoidState' state)

instance TrailLike t => TrailLike (MonoidState [t] ()) where
  trailLike loc = add [trailLike loc]

instance SegmentLike t => SegmentLike (MonoidState [t] ()) where
  segmentLike loc = add [segmentLike loc]

infixr 4 @~
(@~) :: At s => Index s -> IxValue s -> s -> s
key @~ val = Lens.at key ?~ val

infix 4 @=
(@=) :: (At s, MonadState s m) => Index s -> IxValue s -> m ()
key @= val = Lens.at key ?= val