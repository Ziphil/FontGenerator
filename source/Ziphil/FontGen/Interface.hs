--


module Ziphil.FontGen.Interface
  ( main
  )
where

import Data.FontGen
import Data.FontGen.Font
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
      [ ("Vr", Vekos.fontRegular)
      , ("Vb", Vekos.fontBold)
      , ("Vt", Vekos.fontThin)
      , ("Vcr", Vekos.fontCondensedRegular)
      , ("Vcb", Vekos.fontCondensedBold)
      , ("Vct", Vekos.fontCondensedThin)
      , ("Ver", Vekos.fontExtendedRegular)
      , ("Veb", Vekos.fontExtendedBold)
      , ("Vet", Vekos.fontExtendedThin)
      , ("Vhr", Vekos.fontHighRegular)
      , ("Vhb", Vekos.fontHighBold)
      , ("Gr", Gilit.fontRegular)
      , ("Gb", Gilit.fontBold)
      , ("Ger", Gilit.fontExtendedRegular)
      , ("Geb", Gilit.fontExtendedBold)
      , ("Gtr", Gilit.fontTriangleRegular)
      , ("Gtb", Gilit.fontTriangleBold)
      , ("Gser", Gilit.fontSprawledExtendedRegular)
      , ("Gseb", Gilit.fontSprawledExtendedBold)
      ]