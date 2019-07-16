{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit
  ( regularFont
  )
where

import Control.Monad.State
import Data.FontGen
import Data.FontGen.FontType
import Data.Version
import Ziphil.FontGen.Gilit.Config
import qualified Ziphil.FontGen.Gilit.Glyph as Glyph
import qualified Ziphil.FontGen.Gilit.Util as Util


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
  config .= with