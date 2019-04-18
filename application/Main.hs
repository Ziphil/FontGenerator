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

outputGlyphs :: FontInfo -> IO ()
outputGlyphs info = createOutputDir info >> renderGlyphs info

outputStrings :: FontInfo -> IO ()
outputStrings info = renderStrings option info strings
  where
    option = RenderOption {fileName = "test", lineGap = 200, scaleRate = 0.05}
    strings = ["gûbumes e tel a kin,", "pariqez a vahax,", "li fay cali jodog."]