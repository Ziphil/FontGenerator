{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Ziphil.FontGen.Vekos.Config
  ( Config, weightConst, stretchConst
  )
where

import Data.FontGen


data Config = Config {_weightConst :: Double, _stretchConst :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''Config

instance Default Config where
  def = Config 1 1