module Main where

import Test.DocTest

main :: IO ()
main = doctest 
     [ "Data/Object/Dynamic/Types.hs"]


---  "-XDeriveDataTypeable"     
---  "-XFlexibleContexts"       
---  "-XFlexibleInstances"      
---  "-XMultiParamTypeClasses"  
---  "-XRankNTypes"             
---  "-XScopedTypeVariables"    
---  "-XTypeFamilies"           