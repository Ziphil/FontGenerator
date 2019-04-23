{-# LANGUAGE FlexibleContexts #-}


module Ziphil.FontGen.Interface.Generate
  ( start
  )
where

import Data.FontGen hiding (start)
import Data.FontGen.FontType
import Data.FontGen.Render
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import Text.Regex.Posix
import Ziphil.FontGen.Interface.Util
import qualified Ziphil.FontGen.Vekos as Vekos


start :: GenerateOption -> RenderOption -> IO ()
start generateOption renderOption = do
  flushStr $ colorInput "<?> "
  line <- getLine
  infos <- return $ parseInfos line
  infosWithIndex <- return $ zipWith (\info i -> (info, (i, length infos))) infos [1 ..]
  flushStrLn ""
  prepareProgress "Generating"
  mapM (generateAll' generateOption) infosWithIndex
  prepareProgress "Rendering"
  mapM (renderStrings' renderOption) infosWithIndex
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

generateAll' :: GenerateOption -> (FontInfo, (Int, Int)) -> IO ()
generateAll' option (info, progress) = do
  updateProgress "Generating" (info ^. fullName) progress
  generateAll option info

renderStrings' :: RenderOption -> (FontInfo, (Int, Int)) -> IO ()
renderStrings' option (info, progress) = do
  updateProgress "Rendering" (info ^. fullName) progress
  renderStrings option info
  
parseInfos :: String -> [FontInfo]
parseInfos string = Map.elems $ Map.filterWithKey check wholeInfos
  where
    check key info = key =~ ("^" <> string <> "$")

wholeInfos :: Map String FontInfo
wholeInfos = Map.fromList list
  where
    list =
      [ ("Vr", Vekos.regularInfo)
      , ("Vb", Vekos.boldInfo)
      , ("Vt", Vekos.thinInfo)
      , ("Vrc", Vekos.regularCondensedInfo)
      , ("Vbc", Vekos.boldCondensedInfo)
      , ("Vtc", Vekos.thinCondensedInfo)
      , ("Vre", Vekos.regularExtendedInfo)
      , ("Vbe", Vekos.boldExtendedInfo)
      , ("Vte", Vekos.thinExtendedInfo)
      ]