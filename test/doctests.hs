module Main where

import Test.DocTest

main :: IO ()
main = doctest ["Data/Object/Dynamic.hs"]
