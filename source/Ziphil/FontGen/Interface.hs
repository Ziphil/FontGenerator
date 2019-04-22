--


module Ziphil.FontGen.Interface
  ( start
  )
where

import Data.FontGen hiding (start)
import Data.FontGen.FontType
import Data.FontGen.Render
import qualified Ziphil.FontGen.Interface.Generate as Generate


start :: GenerateOption -> RenderOption -> IO ()
start generateOption renderOption = do
  Generate.start generateOption renderOption