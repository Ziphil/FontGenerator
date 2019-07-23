{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Ziphil.FontGen.Kaleg.Config
  ( Config, weightConst
  , Given
  , give
  , given
  )
where

import Data.FontGen
import Data.Reflection (Given)
import qualified Data.Reflection as Reflection


data Config = Config {_weightConst :: Double}
  deriving (Eq, Show)

makeFieldsNoPrefix ''Config

instance Default Config where
  def = Config 1

give :: Config -> (Given Config => r) -> r
give = Reflection.give

given :: Given Config => Config
given = Reflection.given