{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos
  ( regularFont
  , boldFont
  , thinFont
  , regularCondensedFont
  , boldCondensedFont
  , thinCondensedFont
  , regularExtendedFont
  , boldExtendedFont
  , thinExtendedFont
  )
where

import Control.Monad.State
import Data.FontGen
import Data.FontGen.FontType
import Data.Version
import Ziphil.FontGen.Vekos.Config
import qualified Ziphil.FontGen.Vekos.Glyph as Glyph
import qualified Ziphil.FontGen.Vekos.Util as Util


config :: Setter Font Font () Config
config = sets config'
  where
    config' func = (metrics .~ give (func ()) Util.metrics) . (glyphs .~ give (func ()) Glyph.glyphs)

commonState :: State Font ()
commonState = do
  family .= "Vekos"
  copyright .= "Copyright 2019 Ziphil"
  version .= makeVersion [1, 1, 0]

regularFont :: Font
regularFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 1

boldFont :: Font
boldFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 1

thinFont :: Font
thinFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 1

regularCondensedFont :: Font
regularCondensedFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 0.85

boldCondensedFont :: Font
boldCondensedFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 0.85

thinCondensedFont :: Font
thinCondensedFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 0.85

regularExtendedFont :: Font
regularExtendedFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 1.25

boldExtendedFont :: Font
boldExtendedFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 1.25

thinExtendedFont :: Font
thinExtendedFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
    stretch .= Extended
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 1.25