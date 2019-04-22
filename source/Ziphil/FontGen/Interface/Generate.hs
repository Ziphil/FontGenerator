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
import Text.Regex.Posix
import Ziphil.FontGen.Interface.Util
import qualified Ziphil.FontGen.Vekos as Vekos


start :: GenerateOption -> RenderOption -> IO ()
start generateOption renderOption = do
  flushStr $ colorInput "<?> "
  line <- getLine
  flushStrLn ""
  mapM (generateAll' generateOption) $ parseInfos line
  mapM (renderStrings' renderOption) $ parseInfos line
  flushStrLn $ colorMessage "@ Done."

generateAll' :: GenerateOption -> FontInfo -> IO ()
generateAll' option info = do
  generateAll option info
  flushStrLn $ colorMessage $ "@ Generating: " <> info ^. fullName

renderStrings' :: RenderOption -> FontInfo -> IO ()
renderStrings' option info = do
  renderStrings option info
  flushStrLn $ colorMessage $ "@ Rendering: " <> info ^. fullName
  
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