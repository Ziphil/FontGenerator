{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.Glyph
  ( RimElem
  , Rim
  , PartElem
  , Part
  , Glyph
  , Glyphs
  , rimBy
  , partBy
  , unite
  , glyphBy
  , glyphsBy
  )
where

import Data.Default.Class
import Data.FontGen.Metrics
import Data.FontGen.Util.Core
import Data.FontGen.Util.State
import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Backend.SVG
import Diagrams.Prelude


type RimElem = Trail V2 Double
type Rim = MonoidState [RimElem] ()

type PartElem = Path V2 Double
type Part = MonoidState [PartElem] ()

type Glyph = Diagram B

type Glyphs = Map Char Glyph

-- リムからリムを生成します。
-- リムを返す多相関数を do 構文内で使った場合に、型変数の曖昧性を排除するのに利用できます。
rimBy :: Rim -> Rim
rimBy = id

-- リムのリストから 1 つのパスから成るパーツを生成します。
-- 生成の際に自動的にパスを閉じるので、リムの始点と終点は同じ点であるようにしてください。
partBy :: Rim -> Part
partBy rims = add [pathFromTrail . closeTrail . mconcat $ execState' rims]

-- 複数のパーツを結合して 1 つのパスから成るパーツにします。
-- 中に空洞がある形をしたパーツを作成するのに利用できます。
unite :: Part -> Part
unite parts = add [mconcat $ execState' parts]

class ReformEnvelope m s where
  reformEnvelope :: m -> s -> Glyph -> Glyph

-- 与えられたメトリクスとスペーシングの情報に従って、出力用にグリフのエンベロープを修正します。
-- 具体的には、左右にスペーシングとして設定された一定量の余白を追加します。
reformEnvelopeFixed :: Metrics -> FixedSpacing -> Glyph -> Glyph
reformEnvelopeFixed metrics spacing glyph = rectEnvelope base size glyph
  where
    base = (0 - spacing ^. leftBearing &| 0 - metrics ^. metricDescent)
    size = (width glyph + spacing ^. leftBearing + spacing ^. rightBearing &| metrics ^. metricEm)

-- 与えられたメトリクスとスペーシングの情報に従って、出力用にグリフのエンベロープを修正します。
-- 具体的には、指定された X 座標を左端とし、指定された横幅になるように左端に空白を追加します。
reformEnvelopeWidth :: Metrics -> WidthSpacing -> Glyph -> Glyph
reformEnvelopeWidth metrics spacing glyph = rectEnvelope base size glyph
  where
    base = (spacing ^. leftEnd &| 0 - metrics ^. metricDescent)
    size = (spacing ^. fixedWidth &| metrics ^. metricEm)

instance ReformEnvelope Metrics FixedSpacing where
  reformEnvelope = reformEnvelopeFixed

instance ReformEnvelope Metrics WidthSpacing where
  reformEnvelope = reformEnvelopeWidth

-- パーツのリストからグリフを生成します。
-- このとき、左右に与えられた長さの分のスペースができるように、グリフのエンベロープも修正します。
glyphBy :: ReformEnvelope m s => m -> s -> Part -> Glyph
glyphBy metrics spacing = reformEnvelope metrics spacing . mconcat . map strokePath . execState'

glyphsBy :: State Glyphs () -> Glyphs
glyphsBy = flip execState Map.empty