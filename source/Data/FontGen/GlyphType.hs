{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.GlyphType
  ( PartTrailElem
  , PartTrail
  , PartElem
  , Part
  , Glyph
  , Glyphs
  , makeTrail
  , makePart
  , concatPart
  , makeGlyphs
  , (>-)
  , Metrics, metricEm, metricAscent, metricDescent
  , FixedSpacing, leftBearing, rightBearing
  , WidthSpacing, leftX, fixedWidth
  , makeGlyph
  )
where

import Control.Monad.State
import Data.Default.Class
import Data.FontGen.Util
import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Backend.SVG
import Diagrams.Prelude


type PartTrailElem = Trail V2 Double
type PartTrail = State [PartTrailElem] ()

type PartElem = Path V2 Double
type Part = State [PartElem] ()

type Glyph = Diagram B
type Glyphs = Map Char Glyph

-- トレイルからトレイルを生成します。
-- トレイルを返す多相関数を do 構文内で使った場合に、型変数の曖昧性を排除するのに利用できます。
makeTrail :: PartTrail -> PartTrail
makeTrail = id

-- トレイルのリストから 1 つのパスから成るパーツを生成します。
-- 生成の際に自動的にパスを閉じるので、トレイルの始点と終点は同じ点であるようにしてください。
makePart :: PartTrail -> Part
makePart trails = modify (++ [pathFromTrail . closeTrail . mconcat $ execState trails []])

-- 複数のパーツを結合して 1 つのパスから成るパーツにします。
-- 中に空洞がある形をしたパーツを作成するのに利用できます。
concatPart :: Part -> Part
concatPart parts = modify (++ [mconcat $ execState parts []])

makeGlyphs :: [(Char, Glyph)] -> Glyphs
makeGlyphs = Map.fromList

class ToChar c where
  toChar :: c -> Char

instance ToChar Char where
  toChar = id

instance ToChar Int where
  toChar = toEnum

-- グリフマップを生成する際に文字とグリフのタプルを作るユーティリティ演算子です。
infix 0 >-
(>-) :: ToChar c => c -> g -> (Char, g)
thing >- glyph = (toChar thing, glyph)

data Metrics = Metrics {_metricEm :: Double, _metricAscent :: Double, _metricDescent :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''Metrics

instance Default Metrics where
  def = Metrics 1000 750 250

data FixedSpacing = FixedSpacing {_leftBearing :: Double, _rightBearing :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''FixedSpacing

instance Default FixedSpacing where
  def = FixedSpacing 0 0

data WidthSpacing = WidthSpacing {_leftX :: Double, _fixedWidth :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''WidthSpacing

instance Default WidthSpacing where
  def = WidthSpacing 0 0

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
    base = (spacing ^. leftX &| 0 - metrics ^. metricDescent)
    size = (spacing ^. fixedWidth &| metrics ^. metricEm)

instance ReformEnvelope Metrics FixedSpacing where
  reformEnvelope = reformEnvelopeFixed

instance ReformEnvelope Metrics WidthSpacing where
  reformEnvelope = reformEnvelopeWidth

-- パーツのリストからグリフを生成します。
-- このとき、左右に与えられた長さの分のスペースができるように、グリフのエンベロープも修正します。
makeGlyph :: ReformEnvelope m s => m -> s -> Part -> Glyph
makeGlyph metrics spacing = reformEnvelope metrics spacing . mconcat . map strokePath . flip execState []