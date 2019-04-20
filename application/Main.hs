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
  generateAll def Vekos.boldInfo
  generateAll def Vekos.regularCondensedInfo
  generateAll def Vekos.boldCondensedInfo
  writeStrings Vekos.regularInfo
  writeStrings Vekos.boldInfo
  writeStrings Vekos.regularCondensedInfo
  writeStrings Vekos.boldCondensedInfo

writeStrings :: FontInfo -> IO ()
writeStrings info = renderStrings option info strings
  where
    strings = ["gûbumés e tél a kìn,", "pâríqèz a vahax,", "li fay cali jodog."]
    option = with &~ do
      fileName .= "test"
      lineGap .= 200
      scaleRate .= 0.05