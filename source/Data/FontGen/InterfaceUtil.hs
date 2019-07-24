--


module Data.FontGen.InterfaceUtil
  ( colorMessage
  , colorError
  , colorInput
  , flushStrLn
  , flushStr
  , cursorUpLine
  , cursorDownLine
  , clearLine
  )
where

import System.Console.Pretty
import System.IO
import Text.Printf


colorMessage :: Pretty a => a -> a
colorMessage = color Cyan

colorError :: Pretty a => a -> a
colorError = color Red

colorInput :: Pretty a => a -> a
colorInput = color Yellow

flushStrLn :: String -> IO ()
flushStrLn string = putStrLn string >> hFlush stdout

flushStr :: String -> IO ()
flushStr string = putStr string >> hFlush stdout

cursorUpLine :: Int -> IO ()
cursorUpLine size = flushStr $ "\x1b[" <> show size <> "F"

cursorDownLine :: Int -> IO ()
cursorDownLine size = flushStr $ "\x1b[" <> show size <> "E"

clearLine :: IO ()
clearLine = flushStr $ "\x1b[2K"