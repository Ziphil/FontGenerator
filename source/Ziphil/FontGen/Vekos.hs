{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Vekos
  ( fontRegular
  , fontBold
  , fontThin
  , fontCondensedRegular
  , fontCondensedBold
  , fontCondensedThin
  , fontExtendedRegular
  , fontExtendedBold
  , fontExtendedThin
  , fontHighRegular
  , fontHighBold
  )
where

import Control.Monad.State
import Data.FontGen
import Data.FontGen.Font
import Data.Version
import Ziphil.FontGen.Vekos.Config
import qualified Ziphil.FontGen.Vekos.Glyph as Glyph
import qualified Ziphil.FontGen.Vekos.GlyphUtil as Util


config :: Setter Font Font () Config
config = sets $ \func -> (metrics .~ give (func ()) Util.metrics) . (glyphs .~ give (func ()) Glyph.glyphs)

commonState :: State Font ()
commonState = do
  family .= "Vekos"
  copyright .= "Copyright 2019 Ziphil"
  version .= makeVersion [1, 2, 0]

fontRegular :: Font
fontRegular = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 1

fontBold :: Font
fontBold = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 1

fontThin :: Font
fontThin = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 1

fontCondensedRegular :: Font
fontCondensedRegular = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 0.85

fontCondensedBold :: Font
fontCondensedBold = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 0.85

fontCondensedThin :: Font
fontCondensedThin = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
    stretch .= Condensed
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 0.85

fontExtendedRegular :: Font
fontExtendedRegular = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 1.25

fontExtendedBold :: Font
fontExtendedBold = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
    stretch .= Extended
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 1.25

fontExtendedThin :: Font
fontExtendedThin = with &~ commonState &~ do
  style .= with &~ do
    weight .= Thin
    stretch .= Extended
  config .= with &~ do
    weightConst .= 0.5
    stretchConst .= 1.25

fontHighRegular :: Font
fontHighRegular = with &~ commonState &~ do
  family <>= " High"
  style .= with &~ do
    weight .= Regular
  config .= with &~ do
    weightConst .= 1
    stretchConst .= 1
    contrastRatio .= 0.2

fontHighBold :: Font
fontHighBold = with &~ commonState &~ do
  family <>= " High"
  style .= with &~ do
    weight .= Bold
  config .= with &~ do
    weightConst .= 1.5
    stretchConst .= 1
    contrastRatio .= 0.2