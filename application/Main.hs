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
      strings <>= ["gÛbÙmêS e TéL a KîN!" , "PâRíQèZ a VâHáX?", "lÌ ʻfÀy CaLi JÔdÒg."]
      strings <>= ["«â, S'e YeR-fItUl….»"]
      strings <>= ["A 2·4 tE 31:05:2019."]
      strings <>= ["cAtEs — OmêL — a'L."]
      strings <>= ["zA sWA Re Lwe"]
      lineGap .= 200
      scaleRate .= 0.05
      modifyStrings

modifyStrings :: State RenderOption ()
modifyStrings = strings %= map ((<> " ") . (" " <>))