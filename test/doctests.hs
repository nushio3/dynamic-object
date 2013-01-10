module Main where

import Test.DocTest

main :: IO ()
main = doctest 
     [ "-XTypeFamilies"           
     , "-XDeriveDataTypeable"           
     , "-XFlexibleContexts"           
     , "-XFlexibleInstances"           
     , "Data/Object/Dynamic/Examples/PointParticle.hs"
     , "Data/Object/Dynamic/Presets.hs"
     , "Data/Object/Dynamic/Types.hs"
     ]

