{-# LANGUAGE NamedFieldPuns #-}


module Data.FontGen.GlyphType
  ( Part
  , PartSegment
  , PartTrail
  , Glyph
  , Glyphs
  , makePart
  , fixVertical
  , addBearing
  , makeGlyph
  , Metrics (..)
  , Weight (..)
  )
where

import Data.FontGen.Util
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Reflection
import Data.Version
import Diagrams.Backend.SVG
import Diagrams.Prelude


type Part = Path V2 Double
type PartSegment = Segment Closed V2 Double
type PartTrail = Trail V2 Double

type Glyph = Diagram B
type Glyphs = Map Char Glyph

-- トレイルのリストからパーツを生成します。
-- 生成の際に自動的にパスを閉じるので、トレイルの始点と終点は同じ点であるようにしてください。
makePart :: [PartTrail] -> Part
makePart = pathFromTrail . closeTrail . mconcat

-- 与えられたディセンダーの深さとボディの高さに従って、出力用にグリフのエンベロープを修正します。
-- あらかじめ、もともとのグリフの原点をベースライン上の最左の位置に設定しておいてください。
fixVertical :: Double -> Double -> Glyph -> Glyph
fixVertical em descent diagram = rectEnvelope ~. base ~^ size $ diagram
  where
    base = (0, -descent)
    size = (width diagram, em)

-- 左右に与えられた長さの分のスペースができるように、グリフのエンベロープを修正します。
addBearing :: Double -> Double -> Glyph -> Glyph
addBearing left right = extrudeLeft left . extrudeRight right

-- パーツのリストからグリフを生成します。
-- このとき、左右に与えられた長さの分のスペースができるように、グリフのエンベロープも修正します。
makeGlyph :: Double -> Double -> Double -> Double -> [Part] -> Glyph
makeGlyph em descent left right = addBearing left right . fixVertical em descent . strokePath . mconcat

data Metrics = Metrics {metricEm :: Double, metricAscent :: Double, metricDescent :: Double}
  deriving (Eq, Show)

data Weight = Thin | ExtraLight | Light | Regular | Medium | SemiBold | Bold | ExtraBold | Heavy
  deriving (Eq, Show, Enum)