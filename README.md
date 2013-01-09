dynamic-object
==============

object-oriented programming in Haskell, with duck typing and dynamic methods


[![Build Status](https://travis-ci.org/nushio3/dynamic-object.png?branch=master)](https://travis-ci.org/nushio3/dynamic-object)


- Comparison: Haskell's data type and dynamic object 


　            |data type | dynamic object
--------------|----------|---------------
member access | records  | lens
polymorphism  | type variable | underlying types
type-safe record access | ✔ |✔ 
put different types into single container | ✖　 |✔  
add record at runtime  | ✖　 |✔ 
add record to single instances |✖　 |✔  
