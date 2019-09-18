{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Kaleg.Glyph
  ( glyphs
  )
where

import Data.FontGen
import Ziphil.FontGen.Kaleg.Config
import Ziphil.FontGen.Kaleg.GlyphUtil
import Ziphil.FontGen.Kaleg.Part
import Ziphil.FontGen.Kaleg.Value


glyphs :: Given Config => Glyphs
glyphs = glyphsBy list
  where
    list = skip