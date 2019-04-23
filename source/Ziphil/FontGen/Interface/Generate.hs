{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Interface.Generate
  ( main
  )
where

import Data.FontGen
import Data.FontGen.FontType
import Data.FontGen.Render
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import Text.Regex.Posix
import Ziphil.FontGen.Interface.Util
import qualified Ziphil.FontGen.Vekos as Vekos


main :: GenerateOption -> RenderOption -> IO ()
main generateOption renderOption = do
  flushStr $ colorInput "<?> "
  line <- getLine
  fonts <- return $ parseFonts line
  fontsWithIndex <- return $ zipWith (\font i -> (font, (i, length fonts))) fonts [1 ..]
  flushStrLn ""
  prepareProgress "Generating"
  mapM (generateAll' generateOption) fontsWithIndex
  prepareProgress "Rendering"
  mapM (renderStrings' renderOption) fontsWithIndex
  flushStrLn $ colorMessage "@ Done."

prepareProgress :: String -> IO ()
prepareProgress message = do
  flushStrLn ""
  updateProgress message "" (0, 0)

updateProgress :: String -> String -> (Int, Int) -> IO ()
updateProgress message item (i, size) = do
  cursorUpLine 1
  clearLine
  flushStrLn $ colorMessage $ "@ " <> message <> ": " <> printf "[%2d/%2d]" i size <> " " <> item

generateAll' :: GenerateOption -> (Font, (Int, Int)) -> IO ()
generateAll' option (font, progress) = do
  updateProgress "Generating" (font ^. fullName) progress
  generateAll option font

renderStrings' :: RenderOption -> (Font, (Int, Int)) -> IO ()
renderStrings' option (font, progress) = do
  updateProgress "Rendering" (font ^. fullName) progress
  renderStrings option font
  
parseFonts :: String -> [Font]
parseFonts string = Map.elems $ Map.filterWithKey check wholeFonts
  where
    check key font = key =~ ("^" <> string <> "$")

wholeFonts :: Map String Font
wholeFonts = Map.fromList list
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
      ]