{-# LANGUAGE NamedFieldPuns #-}


module Ziphil.FontGen.Vekos
  ( regularInfo
  )
where

import Data.FontGen.FontType
import qualified Ziphil.FontGen.Vekos.Glyph as Glyph
import qualified Ziphil.FontGen.Vekos.Param as Param
import Data.Reflection
import Data.Version


regularInfo :: FontInfo
regularInfo = FontInfo {family, weight, version, metrics, glyphs}
  where
    family = "Vekos"
    weight = Regular
    version = makeVersion [0, 0, 0]
    metrics = makeMetrics Param.regularConfig
    glyphs = give Param.regularConfig Glyph.glyphs

makeMetrics :: Param.Config -> Metrics
makeMetrics config = Metrics {metricEm, metricAscent, metricDescent}
  where
    metricEm = give config Param.em
    metricAscent = give config Param.ascent
    metricDescent = give config Param.descent