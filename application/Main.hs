--


module Main where

import Data.FontGen
import Data.FontGen.FontType
import Data.FontGen.Render
import System.FilePath
import qualified Ziphil.FontGen.Vekos as Vekos


main :: IO ()
main = do
  generateAll def Vekos.regularInfo
  writeStrings Vekos.regularInfo

writeStrings :: FontInfo -> IO ()
writeStrings info = renderStrings option info strings
  where
    strings = ["gûbumes e tel a kin,", "pariqez a vahax,", "li fay cali jodog."]
    option = with &~ do
      fileName .= "test"
      lineGap .= 200
      scaleRate .= 0.05