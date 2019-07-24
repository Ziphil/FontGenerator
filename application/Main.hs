--


module Main where

import Data.FontGen
import qualified Data.FontGen.Interface as Interface
import Data.FontGen.Render
import Ziphil.FontGen


main :: IO ()
main = Interface.main generateOption renderOption fonts
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