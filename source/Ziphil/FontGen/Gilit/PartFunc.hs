{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.PartFunc
  ( reflectSide
  )
where

import Control.Arrow
import Data.FontGen
import Ziphil.FontGen.Gilit.Config
import Ziphil.FontGen.Gilit.Value


reflectSide :: Given Config => Part -> Part
reflectSide = reflectX >>> moveOriginBy (-triangleWidth &| 0)