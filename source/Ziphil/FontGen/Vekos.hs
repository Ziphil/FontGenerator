--


module Ziphil.FontGen.Vekos
  ( regularInfo
  )
where

import Data.FontGen
import Data.FontGen.FontType
import Ziphil.FontGen.Vekos.Param.Config
import qualified Ziphil.FontGen.Vekos.Glyph as Glyph
import qualified Ziphil.FontGen.Vekos.Param as Param
import Data.Reflection
import Data.Version


regularInfo :: FontInfo
regularInfo = with &~ do
  family .= "Vekos"
  copyright .= "Copyright 2019 Ziphil"
  version .= makeVersion [0, 0, 0]
  style .= with &~ do
    weight .= Regular
    stretch .= Normal
  metrics .= give regularConfig Param.metrics
  glyphs .= give regularConfig Glyph.glyphs

regularConfig :: Config
regularConfig = with &~ do
  weightConst .= 1
  stretchConst .= 1