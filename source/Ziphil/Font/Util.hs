--


module Ziphil.Font.Util
  ( Part
  , Glyph
  , fixVertical
  , addBearing
  )
where

import Diagrams.Backend.SVG
import Diagrams.Prelude as Diagrams


type Part = Path V2 Double
type Glyph = Diagram B

-- 垂直方向の直径が与えられた長さになるように、グリフのエンベロープを修正します。
-- ディセンダーラインからアセンダーラインまでの長さを渡してください。
fixVertical :: Double -> Glyph -> Glyph
fixVertical height diagram = rectEnvelope origin corner diagram
  where
    corner = r2 (width diagram, height)

-- 左右に与えられた長さの分のスペースができるように、グリフのエンベロープを修正します。
addBearing :: Double -> Double -> Glyph -> Glyph
addBearing left right = extrudeLeft left . extrudeRight right