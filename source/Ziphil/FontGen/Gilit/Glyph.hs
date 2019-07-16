{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Glyph
  ( glyphs
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.Part
import Ziphil.FontGen.Gilit.Util
import Ziphil.FontGen.Gilit.Value


glyphs :: Given Config => Glyphs
glyphs = makeGlyphs list
  where
    list =
      [ 'a' >- glyphUpAt
      , 't' >- glyphUpTal
      , 'f' >- glyphUpFal
      ]

glyphUpAt :: Given Config => Glyph
glyphUpAt = makeGlyph' parts
  where
    parts = 
      [ partLeftUpOblique
      , partRightUpOblique
      , partUpBase
      ]

glyphUpTal :: Given Config => Glyph
glyphUpTal = makeGlyph' parts
  where
    parts = 
      [ partLeftUpOblique
      , partUpBase
      ]

glyphUpFal :: Given Config => Glyph
glyphUpFal = makeGlyph' parts
  where
    parts = 
      [ partRightUpOblique
      , partUpBase
      ]