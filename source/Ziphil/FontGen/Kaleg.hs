{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Kaleg
  ( fontRegular
  , fontBold
  )
where

import Control.Monad.State
import Data.FontGen
import Data.FontGen.Font
import Data.Version
import Ziphil.FontGen.Kaleg.Config
import qualified Ziphil.FontGen.Kaleg.Glyph as Glyph
import qualified Ziphil.FontGen.Kaleg.GlyphUtil as Util


config :: Setter Font Font () Config
config = sets $ \func -> (metrics .~ give (func ()) Util.metrics) . (glyphs .~ give (func ()) Glyph.glyphs)

commonState :: State Font ()
commonState = do
  family .= "Kaleg"
  copyright .= "Copyright 2019 Ziphil"
  version .= makeVersion [0, 0, 0]

fontRegular :: Font
fontRegular = with &~ commonState &~ do
  style .= with &~ do
    weight .= Regular
  config .= with &~ do
    weightConst .= 1

fontBold :: Font
fontBold = with &~ commonState &~ do
  style .= with &~ do
    weight .= Bold
  config .= with &~ do
    weightConst .= 1.5