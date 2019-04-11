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
    metrics = makeMetrics
    glyphs = give Param.regularConfig Glyph.glyphs

makeMetrics :: FontMetrics
makeMetrics = FontMetrics {em, ascent, descent}
  where
    em = give Param.regularConfig Param.mean + give Param.regularConfig Param.descent * 2
    ascent = give Param.regularConfig Param.mean + give Param.regularConfig Param.descent
    descent = give Param.regularConfig Param.descent