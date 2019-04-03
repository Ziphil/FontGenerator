{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Ziphil.Font.Util
  ( (~^)
  , (~.)
  , fromSegment
  , rotateHalfTurn
  , SegmentLike (..)
  , straight'
  , bezier3'
  , Part
  , PartSegment
  , PartTrail
  , Glyph
  , fixVertical
  , addBearing
  )
where

import Diagrams.Backend.SVG
import Diagrams.Prelude
import Ziphil.Util.Core


infixl 9 ~^
(~^) :: (V2 n -> b) -> (n, n) -> b
func ~^ coord = func $ r2 coord

infixl 9 ~.
(~.) :: (P2 n -> b) -> (n, n) -> b
func ~. coord = func $ p2 coord

fromSegment :: TrailLike t => Segment Closed (V t) (N t) -> t
fromSegment = fromSegments . (: [])

-- 与えられた図形を 180° 回転します。
rotateHalfTurn :: (InSpace V2 n t, Floating n, Transformable t) => t -> t
rotateHalfTurn = rotate halfTurn

class (Metric (V s), OrderedField (N s)) => SegmentLike s where
  segmentLike :: Segment Closed (V s) (N s) -> s

instance (Metric v, OrderedField n) => SegmentLike (Segment Closed v n) where
  segmentLike = id

instance (Metric v, OrderedField n) => SegmentLike (Trail v n) where
  segmentLike = fromSegments . (: [])

straight' :: SegmentLike s => Vn s -> s
straight' = segmentLike . straight

bezier3' :: SegmentLike s => Vn s -> Vn s -> Vn s -> s
bezier3' = segmentLike .^^ bezier3

type Part = Path V2 Double
type PartSegment = Segment Closed V2 Double
type PartTrail = Trail V2 Double
type Glyph = Diagram B

-- 与えられたディセンダーの深さとボディの高さに従って、出力用にグリフのエンベロープを修正します。
-- あらかじめ、もともとのグリフの原点をベースライン上の最左の位置に設定しておいてください。
fixVertical :: Double -> Double -> Glyph -> Glyph
fixVertical descent height diagram = rectEnvelope ~. base ~^ size $ diagram
  where
    base = (0, -descent)
    size = (width diagram, height)

-- 左右に与えられた長さの分のスペースができるように、グリフのエンベロープを修正します。
addBearing :: Double -> Double -> Glyph -> Glyph
addBearing left right = extrudeLeft left . extrudeRight right