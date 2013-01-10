dynamic-object
==============

object-oriented programming in Haskell, with duck typing and singleton class features.


[![Build Status](https://travis-ci.org/nushio3/dynamic-object.png?branch=master)](https://travis-ci.org/nushio3/dynamic-object)


- Comparison: Haskell's algebraic data types,  and dynamic object. 


　                               | data type     | HList.Record  |dynamic object
---------------------------------|---------------|---------------|---------------
member key                       | record names  | phantom types | TypeRep
member access                    | record syntax | functions     | lens
polymorphism                     | type variable | built-in      | underlying types
type-safe member access          | ✔             | ✔             | ✔ 
runtime member addition          | ✖             | ✖             | ✔ 
instance specific members        | ✖　           | ✖             | ✔ 
different objects into one list  | ✖　           | ✖             | ✔ 
missing member access            | runtime error | type error    | returns Nothing, or configurable default behavior
value level equality             | ✔ 　          | ✔             | ✖
bijective de/serialization       | ✔ 　          | ✔             | ✖
