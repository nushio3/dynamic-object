module Main where

import Test.DocTest

main :: IO ()
main = doctest 
     [ "-XTypeFamilies"           
     , "Data/Object/Dynamic/Types.hs"
     , "Data/Object/Dynamic/Presets.hs"]

