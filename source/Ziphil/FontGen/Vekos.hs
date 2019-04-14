{-# LANGUAGE NamedFieldPuns #-}


module Ziphil.FontGen.Vekos
  ( regularInfo
  )
where

import Data.FontGen.FontType
import Data.FontGen.GlyphType
import qualified Ziphil.FontGen.Vekos.Glyph as Glyph
import qualified Ziphil.FontGen.Vekos.Param as Param
import Data.Reflection
import Data.Version


regularInfo :: FontInfo
regularInfo = FontInfo {family, style, version, metrics, glyphs}
  where
    family = "Vekos"
    style = Style Regular Upright Normal
    version = makeVersion [0, 0, 0]
    metrics = give Param.regularConfig Param.metrics
    glyphs = give Param.regularConfig Glyph.glyphs