{-# LANGUAGE FlexibleContexts #-}

module Data.FontGen.Interface
  ( main
  )
where

import Control.Monad
import Data.Char
import Data.FontGen
import Data.FontGen.Font
import Data.FontGen.Render
import Data.FontGen.Util.Interface
import qualified Data.Map as Map
import Text.Printf
import Text.Regex.Posix


main :: GenerateOption -> RenderOption -> Fonts -> IO ()
main generateOption renderOption fonts = do
  complete <- inputComplete
  fonts <- inputFonts fonts
  flushStrLn ""
  mainLoop generateOption renderOption complete $ makeIndexedFonts fonts

inputComplete :: IO Bool
inputComplete = do
  flushStr $ colorInput "<?> Generate -> "
  input <- getLine
  case map toUpper input of
    "" -> return False
    'Y' : _ -> return True
    'N' : _ -> return False
    _ -> return False

inputFonts :: Fonts -> IO [Font]
inputFonts fonts = do
  flushStr $ colorInput "<?> Code -> "
  input <- getLine
  parseFonts fonts input

parseFonts :: Fonts -> String -> IO [Font]
parseFonts fonts string = 
  if null string
    then return . Map.elems $ fonts
    else return . Map.elems $ Map.filterWithKey check fonts
  where
    check key font = key =~ ("^" <> string <> "$")

type IndexedFont = (Font, (Int, Int))

makeIndexedFonts :: [Font] -> [IndexedFont]
makeIndexedFonts fonts = zipWith make fonts [1 ..]
  where
    make font index = (font, (index, length fonts))

mainLoop :: GenerateOption -> RenderOption -> Bool -> [IndexedFont] -> IO ()
mainLoop generateOption renderOption complete indexedFonts = do
  when complete . void $ do
    prepareProgress "Generating"
    mapM (generateAll' generateOption) indexedFonts
  prepareProgress "Rendering"
  mapM (renderAll' renderOption) indexedFonts
  flushStrLn $ colorMessage "@ Done."

generateAll' :: GenerateOption -> IndexedFont -> IO ()
generateAll' option (font, progress) = do
  updateProgress "Generating" (font ^. fullName) progress
  generateAll option font

renderAll' :: RenderOption -> IndexedFont -> IO ()
renderAll' option (font, progress) = do
  updateProgress "Rendering" (font ^. fullName) progress
  renderAll option font

prepareProgress :: String -> IO ()
prepareProgress message = do
  flushStrLn ""
  updateProgress message "" (0, 0)

updateProgress :: String -> String -> (Int, Int) -> IO ()
updateProgress message item (index, size) = do
  cursorUpLine 1
  clearLine
  flushStrLn $ colorMessage $ "@ " <> message <> ": " <> printf "[%2d/%2d]" index size <> " " <> item