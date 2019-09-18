{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.FontGen.Metrics
  ( MetricsPoly, metricEm, metricAscent, metricDescent
  , FixedSpacingPoly, leftBearing, rightBearing
  , WidthSpacingPoly, leftEnd, fixedWidth
  , Metrics
  , FixedSpacing
  , WidthSpacing
  , ReformEnvelope (..)
  )
where

import Data.FontGen.Util.Core
import Diagrams.Prelude
import Diagrams.Backend.SVG


data MetricsPoly n = MetricsPoly {_metricEm :: n, _metricAscent :: n, _metricDescent :: n}
  deriving (Eq, Show, Functor)

makeFieldsNoPrefix ''MetricsPoly

instance Applicative MetricsPoly where
  pure val = MetricsPoly val val val 
  MetricsPoly fstFunc sndFunc thdFunc <*> MetricsPoly fstVal sndVal thdVal = MetricsPoly (fstFunc fstVal) (sndFunc sndVal) (thdFunc thdVal)

instance Additive MetricsPoly where
  zero = MetricsPoly 0 0 0

instance Num n => Default (MetricsPoly n) where
  def = MetricsPoly 1000 750 250

data FixedSpacingPoly n = FixedSpacingPoly {_leftBearing :: n, _rightBearing :: n}
  deriving (Eq, Show, Functor)

makeFieldsNoPrefix ''FixedSpacingPoly

instance Applicative FixedSpacingPoly where
  pure val = FixedSpacingPoly val val
  FixedSpacingPoly fstFunc sndFunc <*> FixedSpacingPoly fstVal sndVal = FixedSpacingPoly (fstFunc fstVal) (sndFunc sndVal)

instance Additive FixedSpacingPoly where
  zero = FixedSpacingPoly 0 0

instance Num n => Default (FixedSpacingPoly n) where
  def = FixedSpacingPoly 0 0

data WidthSpacingPoly n = WidthSpacingPoly {_leftEnd :: n, _fixedWidth :: n}
  deriving (Eq, Show, Functor)

makeFieldsNoPrefix ''WidthSpacingPoly

instance Applicative WidthSpacingPoly where
  pure val = WidthSpacingPoly val val
  WidthSpacingPoly fstFunc sndFunc <*> WidthSpacingPoly fstVal sndVal = WidthSpacingPoly (fstFunc fstVal) (sndFunc sndVal)

instance Additive WidthSpacingPoly where
  zero = WidthSpacingPoly 0 0

instance Num n => Default (WidthSpacingPoly n) where
  def = WidthSpacingPoly 0 0

type Metrics = MetricsPoly Double
type FixedSpacing = FixedSpacingPoly Double
type WidthSpacing = WidthSpacingPoly Double

class ReformEnvelope m s where
  reformEnvelope :: m -> s -> Diagram B -> Diagram B

-- 与えられたメトリクスとスペーシングの情報に従って、出力用にグリフのエンベロープを修正します。
-- 具体的には、左右にスペーシングとして設定された一定量の余白を追加します。
reformEnvelopeFixed :: Metrics -> FixedSpacing -> Diagram B -> Diagram B
reformEnvelopeFixed metrics spacing glyph = rectEnvelope base size glyph
  where
    base = (0 - spacing ^. leftBearing &| 0 - metrics ^. metricDescent)
    size = (width glyph + spacing ^. leftBearing + spacing ^. rightBearing &| metrics ^. metricEm)

-- 与えられたメトリクスとスペーシングの情報に従って、出力用にグリフのエンベロープを修正します。
-- 具体的には、指定された X 座標を左端とし、指定された横幅になるように左端に空白を追加します。
reformEnvelopeWidth :: Metrics -> WidthSpacing -> Diagram B -> Diagram B
reformEnvelopeWidth metrics spacing glyph = rectEnvelope base size glyph
  where
    base = (spacing ^. leftEnd &| 0 - metrics ^. metricDescent)
    size = (spacing ^. fixedWidth &| metrics ^. metricEm)

instance ReformEnvelope Metrics FixedSpacing where
  reformEnvelope = reformEnvelopeFixed

instance ReformEnvelope Metrics WidthSpacing where
  reformEnvelope = reformEnvelopeWidth