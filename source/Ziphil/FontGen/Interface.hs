--


module Ziphil.FontGen.Interface
  ( main
  )
where

import Data.FontGen
import Data.FontGen.FontType
import Data.FontGen.Render
import qualified Ziphil.FontGen.Interface.Generate as Generate


main :: GenerateOption -> RenderOption -> IO ()
main generateOption renderOption = do
  Generate.main generateOption renderOption