--


module Ziphil.FontGen.Vekos
  ( regularInfo
  , boldInfo
  , thinInfo
  , regularCondensedInfo
  , boldCondensedInfo
  , thinCondensedInfo
  , regularExtendedInfo
  , boldExtendedInfo
  , thinExtendedInfo
  )
where

import Control.Monad.State
import Data.FontGen
import Data.FontGen.FontType
import Ziphil.FontGen.Vekos.Param.Config
import qualified Ziphil.FontGen.Vekos.Glyph as Glyph
import qualified Ziphil.FontGen.Vekos.Param as Param
import Data.Reflection
import Data.Version


commonState :: State FontInfo ()
commonState = do
  family .= "Vekos"
  copyright .= "Copyright 2019 Ziphil"
  version .= makeVersion [0, 0, 0]

regularInfo :: FontInfo
regularInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
  metrics .= give regularConfig Param.metrics
  glyphs .= give regularConfig Glyph.glyphs

regularConfig :: Config
regularConfig = with &~ do
  weightConst .= 1
  stretchConst .= 1

boldInfo :: FontInfo
boldInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
  metrics .= give boldConfig Param.metrics
  glyphs .= give boldConfig Glyph.glyphs

boldConfig :: Config
boldConfig = with &~ do
  weightConst .= 1.5
  stretchConst .= 1

thinInfo :: FontInfo
thinInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
  metrics .= give thinConfig Param.metrics
  glyphs .= give thinConfig Glyph.glyphs

thinConfig :: Config
thinConfig = with &~ do
  weightConst .= 0.5
  stretchConst .= 1

regularCondensedInfo :: FontInfo
regularCondensedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Condensed
  metrics .= give regularCondensedConfig Param.metrics
  glyphs .= give regularCondensedConfig Glyph.glyphs

regularCondensedConfig :: Config
regularCondensedConfig = with &~ do
  weightConst .= 1
  stretchConst .= 0.85

boldCondensedInfo :: FontInfo
boldCondensedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Condensed
  metrics .= give boldCondensedConfig Param.metrics
  glyphs .= give boldCondensedConfig Glyph.glyphs

boldCondensedConfig :: Config
boldCondensedConfig = with &~ do
  weightConst .= 1.5
  stretchConst .= 0.85

thinCondensedInfo :: FontInfo
thinCondensedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
    stretch .= Condensed
  metrics .= give thinCondensedConfig Param.metrics
  glyphs .= give thinCondensedConfig Glyph.glyphs

thinCondensedConfig :: Config
thinCondensedConfig = with &~ do
  weightConst .= 0.5
  stretchConst .= 0.85

regularExtendedInfo :: FontInfo
regularExtendedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Extended
  metrics .= give regularExtendedConfig Param.metrics
  glyphs .= give regularExtendedConfig Glyph.glyphs

regularExtendedConfig :: Config
regularExtendedConfig = with &~ do
  weightConst .= 1
  stretchConst .= 1.25

boldExtendedInfo :: FontInfo
boldExtendedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Extended
  metrics .= give boldExtendedConfig Param.metrics
  glyphs .= give boldExtendedConfig Glyph.glyphs

boldExtendedConfig :: Config
boldExtendedConfig = with &~ do
  weightConst .= 1.5
  stretchConst .= 1.25

thinExtendedInfo :: FontInfo
thinExtendedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
    stretch .= Extended
  metrics .= give thinExtendedConfig Param.metrics
  glyphs .= give thinExtendedConfig Glyph.glyphs

thinExtendedConfig :: Config
thinExtendedConfig = with &~ do
  weightConst .= 0.5
  stretchConst .= 1.25