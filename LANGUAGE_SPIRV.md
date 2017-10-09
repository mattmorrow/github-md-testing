# language-spirv

`language-spirv` is a Haskell library for working with the SPIR-V compiler
intermediate representation.

## Objectives

1) Decode/encode binary SPIR-V files to/from a module representation.
2) Provide an API to build SPIR-V modules.
3) Test for strict or nonstrict validity of SPIR-V modules.
4) Transform nonstrictly valid modules into strictl valide ones.

## Library Signature

    signature LANGUAGE_SPIRV = sig
    {
      type Context;
      data Config = Config
        {configMagic :: !WORD
        ,configVersion :: !WORD
        ,configSchema :: !WORD};
      type Result = Either Errors;
      type Errors = [Error];
      type Error;
      context :: Config -> Result Context;

      --
      -- (1)
      --
      type Module;
      decode :: Context -> L.ByteString -> Result Module;
      encode :: Context -> Module -> Result L.ByteString;

      --
      -- (2)
      --
      type Build a;
      build :: Context -> Build a -> Result (a, Module);
      instance Monad Build;
      instance Applicative Build;
      instance Functor Build;

      --
      -- (3)
      --
      data Strict = Strict | NonStrict;
      validate :: Context -> Strict -> Module -> Result ();

      --
      -- (4)
      --
      strictify :: Context -> Module -> Result Module;
    };

## Strict and Nonstrict Validity













