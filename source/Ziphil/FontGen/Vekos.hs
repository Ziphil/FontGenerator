{-# LANGUAGE NamedFieldPuns #-}


module Ziphil.FontGen.Vekos
  ( regularInfo
  )
where

import Data.FontGen.FontType
import qualified Ziphil.FontGen.Vekos.Glyph as Glyph
import qualified Ziphil.FontGen.Vekos.Param as Param
import Data.Version


regularInfo :: FontInfo
regularInfo = FontInfo {family, weight, version, metrics, glyphs}
  where
    family = "Vekos"
    weight = Regular
    version = makeVersion [0, 0, 0]
    metrics = makeMetrics
    glyphs = Glyph.glyphs

makeMetrics :: FontMetrics
makeMetrics = FontMetrics {em, ascent, descent}
  where
    em = Param.mean + Param.descent * 2
    ascent = Param.mean + Param.descent
    descent = Param.descent