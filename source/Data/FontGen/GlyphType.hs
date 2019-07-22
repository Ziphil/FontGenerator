{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.GlyphType
  ( RimElem
  , Rim
  , PartElem
  , Part
  , Glyph
  , Glyphs
  , GlyphsState
  , makeRim
  , makePart
  , concatPart
  , makeGlyph
  , (>-)
  , makeGlyphs
  )
where

import Control.Monad.State
import Data.Default.Class
import Data.FontGen.Metrics
import Data.FontGen.MonoidState
import Data.FontGen.Util
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
type GlyphsState = State Glyphs ()

-- リムからリムを生成します。
-- リムを返す多相関数を do 構文内で使った場合に、型変数の曖昧性を排除するのに利用できます。
makeRim :: Rim -> Rim
makeRim = id

-- リムのリストから 1 つのパスから成るパーツを生成します。
-- 生成の際に自動的にパスを閉じるので、リムの始点と終点は同じ点であるようにしてください。
makePart :: Rim -> Part
makePart rims = add [pathFromTrail . closeTrail . mconcat $ execMonoidState' rims]

-- 複数のパーツを結合して 1 つのパスから成るパーツにします。
-- 中に空洞がある形をしたパーツを作成するのに利用できます。
concatPart :: Part -> Part
concatPart parts = add [mconcat $ execMonoidState' parts]

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
makeGlyph metrics spacing = reformEnvelope metrics spacing . mconcat . map strokePath . execMonoidState'

class ToChar c where
  toChar :: c -> Char

instance ToChar Char where
  toChar = id

instance ToChar Int where
  toChar = toEnum

-- グリフマップにグリフを更新する状態を生成します。
infix 0 >-
(>-) :: ToChar c => c -> Glyph -> GlyphsState
thing >- glyph = modify $ Map.insert (toChar thing) glyph

makeGlyphs :: GlyphsState -> Glyphs
makeGlyphs = flip execState Map.empty