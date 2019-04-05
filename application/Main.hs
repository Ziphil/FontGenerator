--


module Main where

import Data.FontGen.Render
import System.FilePath
import qualified Ziphil.FontGen.Vekos as Vekos


main :: IO ()
main = do
  renderGlyphs Vekos.regularInfo
  renderStrings Vekos.regularInfo ["qolamet", "hizelis", "kofet"]