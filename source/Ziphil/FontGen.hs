--


module Ziphil.FontGen
  ( fonts
  )
where

import Data.FontGen
import Data.FontGen.Font
import qualified Ziphil.FontGen.Gilit as Gilit
import qualified Ziphil.FontGen.Kaleg as Kaleg
import qualified Ziphil.FontGen.Vekos as Vekos


fonts :: Fonts
fonts = fontsBy list
  where
    list = do
      "Vr" @= Vekos.fontRegular
      "Vb" @= Vekos.fontBold
      "Vt" @= Vekos.fontThin
      "Vcr" @= Vekos.fontCondensedRegular
      "Vcb" @= Vekos.fontCondensedBold
      "Vct" @= Vekos.fontCondensedThin
      "Ver" @= Vekos.fontExtendedRegular
      "Veb" @= Vekos.fontExtendedBold
      "Vet" @= Vekos.fontExtendedThin
      "Vhr" @= Vekos.fontHighRegular
      "Vhb" @= Vekos.fontHighBold
      "Gr" @= Gilit.fontRegular
      "Gb" @= Gilit.fontBold
      "Ger" @= Gilit.fontExtendedRegular
      "Geb" @= Gilit.fontExtendedBold
      "Gtr" @= Gilit.fontTriangleRegular
      "Gtb" @= Gilit.fontTriangleBold
      "Gser" @= Gilit.fontSprawledExtendedRegular
      "Gseb" @= Gilit.fontSprawledExtendedBold
      "Kr" @= Kaleg.fontRegular
      "Kb" @= Kaleg.fontBold