{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.GlyphType
  ( Part
  , PartSegment
  , PartTrail
  , Glyph
  , Glyphs
  , makePart
  , Metrics, metricEm, metricAscent, metricDescent
  , fixVertical
  , Spacing, leftBearing, rightBearing
  , addBearing
  , makeGlyph
  )
where

import Data.Default.Class
import Data.FontGen.Util
import Data.Map (Map)
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

data Metrics = Metrics {_metricEm :: Double, _metricAscent :: Double, _metricDescent :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''Metrics

instance Default Metrics where
  def = Metrics 1000 750 250

-- 与えられたメトリクスの情報に従って、出力用にグリフのエンベロープを修正します。
-- あらかじめ、もともとのグリフの原点をベースライン上の最左の位置に設定しておいてください。
fixVertical :: Metrics -> Glyph -> Glyph
fixVertical metrics diagram = rectEnvelope base size diagram
  where
    base = (0 &| 0 - metrics ^. metricDescent)
    size = (width diagram &| metrics ^. metricEm)

data Spacing = Spacing {_leftBearing :: Double, _rightBearing :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''Spacing

instance Default Spacing where
  def = Spacing 0 0

-- 与えられたスペーシングの情報に従って、グリフのエンベロープの左右に空白を追加します。
addBearing :: Spacing -> Glyph -> Glyph
addBearing spacing = extrudeLeft (spacing ^. leftBearing) . extrudeRight (spacing ^. rightBearing)

-- パーツのリストからグリフを生成します。
-- このとき、左右に与えられた長さの分のスペースができるように、グリフのエンベロープも修正します。
makeGlyph :: Metrics -> Spacing -> [Part] -> Glyph
makeGlyph metrics spacing = addBearing spacing . fixVertical metrics . strokePath . mconcat