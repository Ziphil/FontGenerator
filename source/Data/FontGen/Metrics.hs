{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.FontGen.Metrics
  ( MetricsPoly, metricEm, metricAscent, metricDescent
  , FixedSpacingPoly, leftBearing, rightBearing
  , WidthSpacingPoly, leftX, fixedWidth
  , Metrics
  , FixedSpacing
  , WidthSpacing
  )
where

import Diagrams.Prelude


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

data WidthSpacingPoly n = WidthSpacingPoly {_leftX :: n, _fixedWidth :: n}
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