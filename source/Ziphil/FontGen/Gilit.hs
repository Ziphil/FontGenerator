{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit
  ( regularFont
  , boldFont
  , regularExtendedFont
  , boldExtendedFont
  , regularExtendedSprawledFont
  , boldExtendedSprawledFont
  )
where

import Control.Monad.State
import Data.FontGen
import Data.FontGen.FontType
import Data.Version
import Ziphil.FontGen.Gilit.Config
import qualified Ziphil.FontGen.Gilit.Glyph as Glyph
import qualified Ziphil.FontGen.Gilit.GlyphUtil as Util


config :: Setter Font Font () Config
config = sets $ \func -> (metrics .~ give (func ()) Util.metrics) . (glyphs .~ give (func ()) Glyph.glyphs)

commonState :: State Font ()
commonState = do
  family .= "Gilit"
  copyright .= "Copyright 2019 Ziphil"
  version .= makeVersion [0, 0, 0]

regularFont :: Font
regularFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
  config .= with &~ do
    weightConst .= 1
    stretchRatio .= 1
    ascenderRatio .= 0.5

boldFont :: Font
boldFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
  config .= with &~ do
    weightConst .= 1.5
    stretchRatio .= 1
    ascenderRatio .= 0.5

regularExtendedFont :: Font
regularExtendedFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1
    stretchRatio .= 1.5
    ascenderRatio .= 0.5

boldExtendedFont :: Font
boldExtendedFont = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1.5
    stretchRatio .= 1.5
    ascenderRatio .= 0.5

regularExtendedSprawledFont :: Font
regularExtendedSprawledFont = with &~ commonState &~ do
  family <>= " Sprawled"
  style .= with &~ do
    weight .= Regular
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1
    stretchRatio .= 1.5
    ascenderRatio .= 1

boldExtendedSprawledFont :: Font
boldExtendedSprawledFont = with &~ commonState &~ do
  family <>= " Sprawled"
  style .= with &~ do
    weight .= Bold
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1.5
    stretchRatio .= 1.5
    ascenderRatio .= 1