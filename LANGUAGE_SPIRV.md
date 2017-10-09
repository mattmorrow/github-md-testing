# language-spirv

`language-spirv` is a Haskell library for working with the SPIR-V compiler
intermediate representation.

## Objectives

1) Decode/encode binary SPIR-V files to/from a module representation.
2) Provide an API to build SPIR-V modules.
3) Test for strict or nonstrict validity of SPIR-V modules.
4) Transform nonstrictly valid modules into strictly valid ones.

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


## `decode`/`encode`






## `build`








## `validate`

### Strict and Nonstrict Validity

* Strict Validity = The module is valid by the rules of SPIR-V.

* Nonstrict Validity = The following rules of SPIR-V are relaxed:
    1)
    2)
    3)
    4)
    5)








## `strictify`

```haskell
-- | Given the basicblock graph for a function, along with the entry block,
-- return the nodes sorted in a way that respects dominance.
layoutBB :: Graph -> Node -> Result [Node]
layoutBB g root = __FIXME("layoutBB")

-- | Toposorts the SCCs of the {Type,Constant} graph, asserts any cyclic
-- SCC contains ONLY Types and NO Constants, asserts that all cycles pass
-- through an @OpTypePointer@ node, and inserts @OpForwardPointer@s as
-- appropriate for cyclic SCCs.
layoutTC
  :: Graph    -- Types and Constants
  -> NodeSet  -- ALL Types
  -> NodeSet  -- ONLY Ptr Types
  -> Result ([Node], [Node])
layoutTC g allTs ptrTs = __FIXME("layoutTC")

-- | Asserts the GlobalVariable graph is acyclic, and returns a toposort.
layoutGV :: Graph -> Result [Node]
layoutGV = __FIXME("layoutGV")

-- | Asserts that the DeclarationGroup graph is acyclic, returns
-- DecorationGroups flattened in the sense that no DecorationGroup contains
-- another DecorationGroup. The returned list's order does not matter since
-- all @OpDecorationGroup@s are allowed to preceed all @OpGroupDecorate@ and
-- @OpGroupMemberDecorate@s, and this is what we do.
layoutDG :: Graph -> Result [(Node, [Node])]
layoutDG = __FIXME("layoutDG")
```




