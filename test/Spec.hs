--


module Main where

import Test.Hspec
import Ziphil.Util.Test


test :: SpecWith ()
test = describe "" $ return ()

main :: IO ()
main = hspec test