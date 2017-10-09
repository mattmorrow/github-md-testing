# language-spirv-analyze

`language-spirv-analyze` is a Haskell library that builds on `language-spirv`.

## Objectives

1) Provide a DB datatype that represents the semantic entities of
    a SPIR-V module, and an API to build a DB from a module and flatten
    a DB into a module.
2) Provide an API to merge and split modules (DBs).

## Library Signature

    signature LANGUAGE_SPIRV = sig
    {
      --
      -- (1)
      --
      type DB;
      analyze :: Context -> Module -> Result DB;
      flatten :: Context -> DB -> Result Module;

      --
      -- (2)
      --
      type Roots;
      link :: Context -> [DB] -> Result DB;
      extract :: Context -> DB -> Roots -> Result DB;
    };

