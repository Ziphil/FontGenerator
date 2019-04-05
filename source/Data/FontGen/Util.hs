{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.FontGen.Util
  ( (~^)
  , (~.)
  , rotateHalfTurn
  , SegmentLike (..)
  , straight'
  , bezier3'
  , bezier2'
  )
where

import Diagrams.Prelude


infixl 9 ~^
(~^) :: (V2 n -> b) -> (n, n) -> b
func ~^ coord = func $ r2 coord

infixl 9 ~.
(~.) :: (P2 n -> b) -> (n, n) -> b
func ~. coord = func $ p2 coord

-- 与えられた図形を 180° 回転します。
rotateHalfTurn :: (InSpace V2 n t, Floating n, Transformable t) => t -> t
rotateHalfTurn = rotate halfTurn

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