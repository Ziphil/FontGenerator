--


module Main where

import Data.FontGen
import Data.FontGen.Render
import System.FilePath
import qualified Ziphil.FontGen.Interface as Interface


main :: IO ()
main = Interface.main generateOption renderOption
  where
    generateOption = def
    renderOption = with &~ do
      strings .= 
        [ "gûbùmês e tél a kîn!"
        , "pâríqèz a vâháx?"
        , "lì ʻfày cali jôdòg."
        , "«â, s'e yer-fitul….»"
        , "a 2·4 te 31:05:2019."
        , "cates — omêl — a'l."
        ]
      fileName .= "test"
      lineGap .= 200
      scaleRate .= 0.05