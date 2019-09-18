{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Gilit.PartFunc
  ( reflectX'
  , transpose
  )
where

import Control.Arrow
import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.Value


reflectX' :: Given Config => Part -> Part
reflectX' = reflectX >>> moveOriginBy (-triangleWidth &| 0)

-- 上に尖った三角形状の文字のパーツを反転させ、下に尖った三角形状の文字のパーツに変換します。
transpose :: Given Config => Part -> Part
transpose = reflectY >>> moveOriginBy (0 &| -triangleHeight)