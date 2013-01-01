module Main where

import Test.DocTest

main :: IO ()
main = doctest 
     [ "-XDeriveDataTypeable"
     , "-XTypeFamilies"
     , "Data/Object/Dynamic/Types.hs"]
