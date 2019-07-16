{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Interface.Generate
  ( main
  )
where

import Control.Monad
import Data.FontGen
import Data.FontGen.FontType
import Data.FontGen.Render
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import Text.Regex.Posix
import Ziphil.FontGen.Interface.Util


main :: GenerateOption -> RenderOption -> Map String Font -> IO ()
main generateOption renderOption fonts = do
  simple <- inputSimple
  fonts <- inputFonts fonts
  flushStrLn ""
  mainLoop generateOption renderOption simple $ makeIndexedFonts fonts

inputSimple :: IO Bool
inputSimple = do
  flushStr $ colorInput "<?> Simple -> "
  input <- getLine
  case input of
    "" -> return True
    'Y' : _ -> return True
    'N' : _ -> return False
    _ -> return True

inputFonts :: Map String Font -> IO [Font]
inputFonts fonts = do
  flushStr $ colorInput "<?> Name -> "
  input <- getLine
  parseFonts fonts input

parseFonts :: Map String Font -> String -> IO [Font]
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
mainLoop generateOption renderOption simple indexedFonts = do
  unless simple . void $ do
    prepareProgress "Generating"
    mapM (generateAll' generateOption) indexedFonts
  prepareProgress "Rendering"
  mapM (renderStrings' renderOption) indexedFonts
  flushStrLn $ colorMessage "@ Done."

generateAll' :: GenerateOption -> IndexedFont -> IO ()
generateAll' option (font, progress) = do
  updateProgress "Generating" (font ^. fullName) progress
  generateAll option font

renderStrings' :: RenderOption -> IndexedFont -> IO ()
renderStrings' option (font, progress) = do
  updateProgress "Rendering" (font ^. fullName) progress
  renderStrings option font

prepareProgress :: String -> IO ()
prepareProgress message = do
  flushStrLn ""
  updateProgress message "" (0, 0)

updateProgress :: String -> String -> (Int, Int) -> IO ()
updateProgress message item (index, size) = do
  cursorUpLine 1
  clearLine
  flushStrLn $ colorMessage $ "@ " <> message <> ": " <> printf "[%2d/%2d]" index size <> " " <> item