{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Kaleg.Part
  ( ascent
  , descent
  , em
  , actualAscent
  , actualDescent
  , actualEm
  )
where

import Data.FontGen
import Ziphil.FontGen.Kaleg.Config
import Ziphil.FontGen.Kaleg.Value


ascent :: Given Config => Double
ascent = mean + descent

em :: Given Config => Double
em = ascent + descent

actualDescent :: Given Config => Double
actualDescent = descent + extraDescent

actualAscent :: Given Config => Double
actualAscent = ascent + extraAscent

actualEm :: Given Config => Double
actualEm = actualDescent + actualAscent