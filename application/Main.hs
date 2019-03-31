--


module Main where

import Ziphil.Font.Core
import qualified Ziphil.Font.Vekos.Glyph as Vekos


main :: IO ()
main = renderAllGlyphs "out" Vekos.correspondence