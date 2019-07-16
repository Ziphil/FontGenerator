--


module Ziphil.FontGen.Interface
  ( main
  )
where

import Data.FontGen
import Data.FontGen.FontType
import Data.FontGen.Render
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Ziphil.FontGen.Gilit as Gilit
import qualified Ziphil.FontGen.Interface.Generate as Generate
import qualified Ziphil.FontGen.Vekos as Vekos


main :: GenerateOption -> RenderOption -> IO ()
main generateOption renderOption = Generate.main generateOption renderOption fonts

fonts :: Map String Font
fonts = Map.fromList list
  where
    list =
      [ ("Vr", Vekos.regularFont)
      , ("Vb", Vekos.boldFont)
      , ("Vt", Vekos.thinFont)
      , ("Vrc", Vekos.regularCondensedFont)
      , ("Vbc", Vekos.boldCondensedFont)
      , ("Vtc", Vekos.thinCondensedFont)
      , ("Vre", Vekos.regularExtendedFont)
      , ("Vbe", Vekos.boldExtendedFont)
      , ("Vte", Vekos.thinExtendedFont)
      , ("Gr", Gilit.regularFont)
      ]