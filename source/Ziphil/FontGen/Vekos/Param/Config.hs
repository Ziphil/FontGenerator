{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Ziphil.FontGen.Vekos.Param.Config
  ( Config, weightConst, stretchConst
  )
where

import Control.Lens
import Data.Default.Class


data Config = Config {_weightConst :: Double, _stretchConst :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''Config

instance Default Config where
  def = Config 1 1