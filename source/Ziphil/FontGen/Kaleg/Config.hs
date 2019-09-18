{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ziphil.FontGen.Kaleg.Config
  ( EdgeShape (..)
  , Config, weightConst, edgeShapeVal
  , Given
  , give
  , given
  )
where

import Data.FontGen
import Data.Reflection (Given)
import qualified Data.Reflection as Reflection


data EdgeShape = Miter | Round | Bevel
  deriving (Eq, Show)

data Config = Config {_weightConst :: Double, _edgeShapeVal :: EdgeShape}
  deriving (Eq, Show)

makeFieldsNoPrefix ''Config

instance Default Config where
  def = Config 1 Miter

give :: Config -> (Given Config => r) -> r
give = Reflection.give

given :: Given Config => Config
given = Reflection.given