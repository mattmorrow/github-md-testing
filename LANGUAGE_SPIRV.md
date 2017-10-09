# language-spirv

`language-spirv` is a Haskell library with the following objectives:

1) Decode/encode binary SPIR-V files to/from a module representation.
2) Provide an API to build SPIR-V modules.
3) Test for strict or nonstrict validity of SPIR-V modules.
4) Transform nonstrictly valid modules into strictl valide ones.
5) Provide a DB datatype that represents the semantic entities of
    a SPIR-V module, and an API to build a DB from a module and flatten
    a DB into a module.
6) Provide an API to merge and split modules (DBs).






## Strict and Nonstrict Validity










