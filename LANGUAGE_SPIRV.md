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


## `ModInfo`

```haskell
data ModuleGraphs a = ModuleGraphs
  {
   mgs_IdPart       :: IdPart ()
  ,mgs_ID_T_Pointer :: IxMap ID ()
  ,mgs_ID_DG        :: Graph ID   -- (DecoGroup, _)
  ,mgs_ID_T_C       :: Graph ID   -- (Type|Constant,Type|Constant)
  ,mgs_ID_VG        :: Graph ID   -- (GVar,GVar)
  ,mgs_ID_B         :: Graph ID   -- (Block,Block)
  ,mgs_ID_F_Entry   :: IxMap ID (Ix ID)
  }
data IdKind
  = IDK_X     -- ^ SSA Name
  | IDK_VL    -- ^ OpVariable (Local)
  | IDK_B     -- ^ OpLabel
  | IDK_FP    -- ^ OpFunctionParameter
  | IDK_F     -- ^ OpFunction
  | IDK_VG    -- ^ OpVariable (Global)
  | IDK_C     -- ^ Constant
  | IDK_U     -- ^ OpUndef
  | IDK_T     -- ^ Type
  | IDK_DG    -- ^ OpDecorationGroup
  | IDK_S     -- ^ OpString
  | IDK_EII   -- ^ OpExtInstImport
data IdPart a = IdPart
  {
   idPartX    :: IxMap ID a   -- ^ SSA Name
  ,idPartVL   :: IxMap ID a   -- ^ OpVariable (Local)
  ,idPartB    :: IxMap ID a   -- ^ OpLabel
  ,idPartFP   :: IxMap ID a   -- ^ OpFunctionParameter
  ,idPartF    :: IxMap ID a   -- ^ OpFunction
  ,idPartVG   :: IxMap ID a   -- ^ OpVariable (Global)
  ,idPartC    :: IxMap ID a   -- ^ Constant
  ,idPartU    :: IxMap ID a   -- ^ OpUndef
  ,idPartT    :: IxMap ID a   -- ^ Type
  ,idPartDG   :: IxMap ID a   -- ^ OpDecorationGroup
  ,idPartS    :: IxMap ID a   -- ^ OpString
  ,idPartEII  :: IxMap ID a   -- ^ OpExtInstImport
  }
```


```haskell
shareSingleOutputCyclic
  :: (Ord k)
  => IxMap a (k, [Ix a])
  -> Partition a a
```

```haskell
shareMultiOutputCyclic
  :: (Ord k)
  => IxMap a (k, [Ix b], [Ix b])
  -> (Partition a a, Partition b b)
shareMultiOutputCyclic a
  | (b,c,d) <- step1 a
  , e <- shareSingleOutputCyclic d
  = step3 (b,c,e)
  where step1
          :: IxMap a (k, [Ix b], [Ix b])
          -> (Trans () a, Trans () b, IxMap () ((Int, k), [Ix ()]))
        step3
          :: (Trans () a, Trans () b, Partition () ())
          -> (Partition a a, Partition b b)
        step1 = __FIXME("shareMultiOutputCyclic_step1")
        step3 = __FIXME("shareMultiOutputCyclic_step3")
```





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
layoutBB :: Graph ID -> Ix ID -> Result [Ix ID]

-- | Toposorts the SCCs of the {Type,Constant} graph, asserts any cyclic
-- SCC contains ONLY Types and NO Constants, asserts that all cycles pass
-- through an @OpTypePointer@ node, and inserts @OpForwardPointer@s as
-- appropriate for cyclic SCCs.
layoutTC
  :: Graph ID -- Types and Constants
  -> NodeSet  -- ALL Types
  -> NodeSet  -- ONLY Ptr Types
  -> Result ([Node], [Node])

-- | Asserts the GlobalVariable graph is acyclic, and returns a toposort.
layoutGV :: Graph -> Result [Node]

-- | Asserts that the DeclarationGroup graph is acyclic, returns
-- DecorationGroups flattened in the sense that no DecorationGroup contains
-- another DecorationGroup. The returned list's order does not matter since
-- all @OpDecorationGroup@s are allowed to preceed all @OpGroupDecorate@ and
-- @OpGroupMemberDecorate@s, and this is what we do.
layoutDG :: Graph -> Result [(Node, [Node])]
```




