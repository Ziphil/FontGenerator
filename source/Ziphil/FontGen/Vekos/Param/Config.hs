{-# LANGUAGE NamedFieldPuns #-}


module Ziphil.FontGen.Vekos.Param.Config
  ( Config (..)
  , regularConfig
  )
where


data Config = Config {weightConst :: Double}
  deriving (Eq, Show)

regularConfig :: Config
regularConfig = Config {weightConst = 1}