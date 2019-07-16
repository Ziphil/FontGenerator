{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Gilit.Value
  ( descent
  , mean
  , ascent
  , em
  , actualDescent
  , actualAscent
  , actualEm
  , bearing
  )
where

import Data.FontGen
import Ziphil.FontGen.Gilit.Config


-- ディセンダーラインの深さを表します。
-- このフォントではディセンダー部分とアセンダー部分の高さが同じなので、アセンダーラインとミーンラインの距離としても利用されます。
descent :: Given Config => Double
descent = 250

-- ミーンラインの高さを表します。
mean :: Given Config => Double
mean = 500

ascent :: Given Config => Double
ascent = mean + descent

em :: Given Config => Double
em = ascent + descent

actualDescent :: Given Config => Double
actualDescent = descent + 40

actualAscent :: Given Config => Double
actualAscent = ascent + 10

actualEm :: Given Config => Double
actualEm = actualDescent + actualAscent

bearing :: Given Config => Double
bearing = 0