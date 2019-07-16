{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Ziphil.FontGen.Gilit.Config
  ( Config
  , Given
  , give
  , given
  )
where

import Data.FontGen
import Data.Reflection (Given)
import qualified Data.Reflection as Reflection


data Config = Config
  deriving (Eq, Show)

makeFieldsNoPrefix ''Config

instance Default Config where
  def = Config

give :: Config -> (Given Config => r) -> r
give = Reflection.give

given :: Given Config => Config
given = Reflection.given