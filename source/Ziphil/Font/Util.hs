{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Ziphil.Font.Util
  ( ($^)
  , ($.)
  , turn
  , reverse
  , invert
  , Part
  , Glyph
  , fixVertical
  , addBearing
  )
where

import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (turn)
import Prelude hiding (reverse)


infixl 9 $^
($^) :: (V2 n -> b) -> (n, n) -> b
func $^ coord = func $ r2 coord

infixl 9 $.
($.) :: (P2 n -> b) -> (n, n) -> b
func $. coord = func $ p2 coord

-- 与えられた図形を 180° 回転します。
turn :: (InSpace V2 n t, Transformable t, Floating n) => t -> t
turn = rotate (180 @@ deg)

-- 与えられた図形を左右反転します。
reverse :: (InSpace v n t, R2 v, Fractional n, Transformable t) => t -> t
reverse = scaleX (-1)

-- 与えられた図形を上下反転します。
invert :: (InSpace v n t, R2 v, Fractional n, Transformable t) => t -> t
invert = scaleY (-1)

type Part = Path V2 Double
type Glyph = Diagram B

-- 与えられたディセンダーの深さとボディの高さに従って、出力用にグリフのエンベロープを修正します。
-- あらかじめ、もともとのグリフの原点をベースライン上の最左の位置に設定しておいてください。
fixVertical :: Double -> Double -> Glyph -> Glyph
fixVertical descender height diagram = rectEnvelope $. base $^ size $ diagram
  where
    base = (0, -descender)
    size = (width diagram, height)

-- 左右に与えられた長さの分のスペースができるように、グリフのエンベロープを修正します。
addBearing :: Double -> Double -> Glyph -> Glyph
addBearing left right = extrudeLeft left . extrudeRight right