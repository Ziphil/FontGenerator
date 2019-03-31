--


module Main where

import System.FilePath
import Ziphil.Font.Render
import qualified Ziphil.Font.Vekos.Glyph as Vekos


main :: IO ()
main = do
  renderAllGlyphs "out" Vekos.correspondence
  renderString ("out" </> "test" <.> "svg") "sztdkgfvpbcqxjlrnmyhaeiou" Vekos.correspondence