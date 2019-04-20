--


module Main where

import Data.FontGen
import Data.FontGen.FontType
import Data.FontGen.Render
import System.FilePath
import qualified Ziphil.FontGen.Vekos as Vekos


main :: IO ()
main = do
  outputGlyphs Vekos.regularInfo
  outputStrings Vekos.regularInfo
  writeCode def Vekos.regularInfo
  generateFont def Vekos.regularInfo
  return ()

outputGlyphs :: FontInfo -> IO ()
outputGlyphs info = createOutputDir info >> renderGlyphs info

outputStrings :: FontInfo -> IO ()
outputStrings info = renderStrings option info strings
  where
    strings = ["gûbumes e tel a kin,", "pariqez a vahax,", "li fay cali jodog."]
    option = with &~ do
      fileName .= "test"
      lineGap .= 200
      scaleRate .= 0.05