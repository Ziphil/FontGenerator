{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}


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


config :: Setter FontInfo FontInfo () Config
config = sets config'
  where
    config' func = (metrics .~ give @Config (func ()) Param.metrics) . (glyphs .~ give (func ()) Glyph.glyphs)

commonState :: State FontInfo ()
commonState = do
  family .= "Vekos"
  copyright .= "Copyright 2019 Ziphil"
  version .= makeVersion [1, 0, 0]

regularInfo :: FontInfo
regularInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 1

boldInfo :: FontInfo
boldInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 1

thinInfo :: FontInfo
thinInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 1

regularCondensedInfo :: FontInfo
regularCondensedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 0.85

boldCondensedInfo :: FontInfo
boldCondensedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 0.85

thinCondensedInfo :: FontInfo
thinCondensedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 0.85

regularExtendedInfo :: FontInfo
regularExtendedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 1.25

boldExtendedInfo :: FontInfo
boldExtendedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 1.25

thinExtendedInfo :: FontInfo
thinExtendedInfo = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
    stretch .= Extended
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 1.25