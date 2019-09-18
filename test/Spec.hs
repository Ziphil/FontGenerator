--

module Main where

import Test.Hspec


test :: SpecWith ()
test = describe "" $ return ()

main :: IO ()
main = hspec test