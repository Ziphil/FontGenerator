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
        [ " gÛbÙmêS e TéL a KîN! "
        , " PâRíQèZ a VâHáX? "
        , " lÌ ʻfÀy CaLi JÔdÒg. "
        , " «â, S'e YeR-fItUl….» "
        , " A 2·4 tE 31:05:2019. "
        , " cAtEs — OmêL — a'L. "
        , " zA sWA Re Lwe "
        ]
      fileName .= "test"
      lineGap .= 200
      scaleRate .= 0.05