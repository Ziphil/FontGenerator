{-# LANGUAGE FlexibleContexts #-}

module Ziphil.FontGen.Kaleg.Value
  ( descent
  , mean
  , extraAscent
  , extraDescent
  , bearing
  , edgeShape
  )
where

import Data.FontGen
import Ziphil.FontGen.Kaleg.Config


descent :: Given Config => Double
descent = 250

mean :: Given Config => Double
mean = 500

extraAscent :: Given Config => Double
extraAscent = 0

extraDescent :: Given Config => Double
extraDescent = 0

bearing :: Given Config => Double
bearing = 0

edgeShape :: Given Config => EdgeShape
edgeShape = edgeShapeVal'
  where
    edgeShapeVal' = given ^. edgeShapeVal