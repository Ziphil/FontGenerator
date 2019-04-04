--


module Data.FontGen.GlyphType
  ( Part
  , PartSegment
  , PartTrail
  , Glyph
  , Glyphs
  , fixVertical
  , addBearing
  )
where

import Data.FontGen.Util
import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Backend.SVG
import Diagrams.Prelude


type Part = Path V2 Double
type PartSegment = Segment Closed V2 Double
type PartTrail = Trail V2 Double

type Glyph = Diagram B
type Glyphs = Map Char Glyph

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