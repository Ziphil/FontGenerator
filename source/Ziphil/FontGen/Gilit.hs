{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Gilit
  ( fontRegular
  , fontBold
  , fontExtendedRegular
  , fontExtendedBold
  , fontTriangleRegular
  , fontTriangleBold
  , fontSprawledExtendedRegular
  , fontSprawledExtendedBold
  )
where

import Control.Monad.State
import Data.FontGen
import Data.FontGen.Font
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
  version .= makeVersion [1, 2, 1]

fontRegular :: Font
fontRegular = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
  config .= with &~ do
    weightConst .= 1
    stretchRatio .= 1
    ascenderRatio .= 0.5

fontBold :: Font
fontBold = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
  config .= with &~ do
    weightConst .= 1.5
    stretchRatio .= 1
    ascenderRatio .= 0.5

fontExtendedRegular :: Font
fontExtendedRegular = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1
    stretchRatio .= 1.5
    ascenderRatio .= 0.5

fontExtendedBold :: Font
fontExtendedBold = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1.5
    stretchRatio .= 1.5
    ascenderRatio .= 0.5

fontTriangleRegular :: Font
fontTriangleRegular = with &~ commonState &~ do
  family <>= " Triangle"
  style .= with &~ do
    weight .= Regular
  config .= with &~ do
    weightConst .= 1
    stretchRatio .= 2 / sqrt 3
    ascenderRatio .= 0.5

fontTriangleBold :: Font
fontTriangleBold = with &~ commonState &~ do
  family <>= " Triangle"
  style .= with &~ do
    weight .= Bold
  config .= with &~ do
    weightConst .= 1.5
    stretchRatio .= 2 / sqrt 3
    ascenderRatio .= 0.5

fontSprawledExtendedRegular :: Font
fontSprawledExtendedRegular = with &~ commonState &~ do
  family <>= " Sprawled"
  style .= with &~ do
    weight .= Regular
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1
    stretchRatio .= 1.5
    ascenderRatio .= 1

fontSprawledExtendedBold :: Font
fontSprawledExtendedBold = with &~ commonState &~ do
  family <>= " Sprawled"
  style .= with &~ do
    weight .= Bold
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1.5
    stretchRatio .= 1.5
    ascenderRatio .= 1