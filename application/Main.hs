--


module Main where

import Data.FontGen
import Data.FontGen.Render
import System.FilePath
import qualified Ziphil.FontGen.Interface as Interface


main :: IO ()
main = Interface.start generateOption renderOption strings
  where
    strings = ["gûbùmês e tél a kîn!", "pâríqèz a vâháx?", "lì ʻfày cali jôdòg.", "«â, e yer-fitul al'atis.»"]
    generateOption = def
    renderOption = with &~ do
      fileName .= "test"
      lineGap .= 200
      scaleRate .= 0.05