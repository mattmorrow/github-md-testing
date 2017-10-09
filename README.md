# github-md-testing




## Libraries

1) mm-compiler-base
2) language-spirv



## NOTES

* github-md-testing/README.md
    * (this file)

* NOTES.md
    * signature COMPILER
    * signature DRIVER
    * signature IR
    * signature ExternalIR
    * signature PASS
    * signature COSTMODEL
    * signature ANALYSIS
    * signature TRANSFORM
    * signature A_GVN_SCVP
    * signature XXX_A_GVN
    * signature XXX_A_SCVP
    * signature GVN
    * signature GlobalValueNumbering
    * signature SCP
    * signature SparseConditionalPropagation
    * signature A_MemSSA
    * signature A_DomPDomTree
    * signature A_Liveness
    * signature A_Alias_Alias
    * signature A_Alias_PointsTo
    * signature A_Mem_ReachingDefs
    * signature A_LNF
    * signature X_FRE
    * signature X_PRE
    * signature X_JumpThread
    * signature X_Specialize
    * signature X_Inline
    * signature X_Outline
    * signature X_MemSSA_Into
    * signature X_MemSSA_OutOf
    * signature C_GVN_SCVP
    * signature C_FRE
    * signature C_PRE
    * signature C_JumpThread
    * signature C_Inline_Outline
    * signature TOTAL_COSTMODEL
    * signature PARTIAL_COSTMODEL
    * signature COSTMODEL_Z
    * signature COSTMODEL_R
    * signature COSTMODEL_Z2
    * signature COSTMODEL_R2

* README_mm-compiler.md
    * signature MM_COMPILER_LIB
    * signature XXX
    * doItAll :: CFG -> m ()

* ALL_mm-compiler_MM_other_files_too.hs
    * MM.Compiler.Types

* LEM_LESSON.md
* OPS.md
* FIR.md
* SPIRV-Tools.md

* language-spirv/NOTES_README.md


* language-spirv/README.md
    * Objectives
    * Library Signature
    * TOORG












-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

* Things
    1) Acyclic to one-leveled
        1) ({DG},_) -> ({DG},not({DG}))
    2) DFA minimization
        1) ({DG},not({DG}))
        2) {T,C,U}^2
    2) Sort by DomTree RevPostDFN
        1) {B}^2

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------



-- //////////////////////////////////////
-- //////////////////////////////////////
-- //////////////////////////////////////
data GraphsOfNote
  = G_Type_Constant         -- only Types can be in SCCs. @OpTypeForwardPointer@.
  | G_GlobalVariable        -- assert(ACYCLIC)
  | G_DecorationGroup       -- assert(ACYCLIC)
  | G_Label                 -- domSort(BBs)
  __D_BE
idLinks :: [(IdKind, [IdKind])]
idLinks =
  [(SSAIdK,              [SSAIdK,LabelIdK,FunParamIdK,FunctionIdK,GlobalVariableIdK,ConstantIdK,TypeIdK,ExtInstImportIdK])
  ,(LabelIdK,            [LabelIdK])
  ,(FunParamIdK,         [TypeIdK])
  ,(FunctionIdK,         [TypeIdK])
  ,(GlobalVariableIdK,   [TypeIdK,ConstantIdK,GlobalVariableIdK])
  ,(ConstantIdK,         [TypeIdK,ConstantIdK])
  ,(TypeIdK,             [TypeIdK,ConstantIdK])
  ,(DecorationGroupIdK,  [SSAIdK,LabelIdK,FunParamIdK,FunctionIdK,GlobalVariableIdK,ConstantIdK,TypeIdK,DecorationGroupIdK,StringIdK,ExtInstImportIdK])
  ,(StringIdK,           [])
  ,(ExtInstImportIdK,    [])]
-- //////////////////////////////////////
-- //////////////////////////////////////
-- //////////////////////////////////////
data ConstantTag = ConstantTag __D_BE
data ConstantKey = ConstantKey __D
data Constant = Constant __D
data TypeTag    -- nothing
-- {{{
  = TypeVoidTag
  | TypeBoolTag
  | TypeIntTag
  | TypeFloatTag
  | TypeVectorTag
  | TypeMatrixTag
  | TypeArrayTag
  | TypeRuntimeArrayTag
  | TypeStructTag
  | TypeOpaqueTag
  | TypePointerTag
  | TypeFunctionTag
  | TypeImageTag
  | TypeSamplerTag
  | TypeSampledImageTag
  | TypeEventTag
  | TypeDeviceEventTag
  | TypeReserveIdTag
  | TypeQueueTag
  | TypePipeTag
  | TypePipeStorageTag
  | TypeNamedBarrierTag
  __D
-- }}}
data TypeKey    -- no Ids
-- {{{
  = TypeVoidKey
  | TypeBoolKey
  | TypeIntKey !WORD !WORD
  | TypeFloatKey !WORD
  | TypeVectorKey !WORD
  | TypeMatrixKey !WORD
  | TypeArrayKey
  | TypeRuntimeArrayKey
  | TypeStructKey
  | TypeOpaqueKey Name
  | TypePointerKey StorageClass
  | TypeFunctionKey
  | TypeImageKey Dim !WORD !WORD !WORD !WORD ImageFormat (Maybe AccessQualifier)
  | TypeSamplerKey
  | TypeSampledImageKey
  | TypeEventKey
  | TypeDeviceEventKey
  | TypeReserveIdKey
  | TypeQueueKey
  | TypePipeKey AccessQualifier
  | TypePipeStorageKey
  | TypeNamedBarrierKey
  __D
-- }}}
data Type       -- no IdResult
-- {{{
  = TypeVoid
  | TypeBool
  | TypeInt             !Int  !Int
  | TypeFloat           !Int
  | TypeVector          Id   !Int
  | TypeMatrix          Id   !Int
  | TypeArray           Id   Id{-Constant:XXXXXXXXXXX-}
  | TypeRuntimeArray    Id
  | TypeStruct          [Id]
  | TypeOpaque          Name
  | TypePointer         StorageClass  Id
  | TypeFunction        Id           [Id]
  -- TypeImage           Id Dim !WORD !WORD !WORD !WORD ImageFormat (Maybe AccessQualifier)
  | TypeImage
      {typeImageSampledType :: Id
      ,typeImageDim :: Dim
      ,typeImageDepth :: !WORD
      ,typeImageArrayed :: !WORD{-{0,1}-}
      ,typeImageMS :: !WORD{-{0,1}-}
      ,typeImageSampled :: !WORD{-{0,1,2}-}
      ,typeImageFormat :: ImageFormat
      ,typeImageAccess :: Maybe AccessQualifier}
  | TypeSampler
  | TypeSampledImage    Id
  | TypeEvent
  | TypeDeviceEvent
  | TypeReserveId
  | TypeQueue
  | TypePipe            AccessQualifier
  | TypePipeStorage
  | TypeNamedBarrier
  __D
-- }}}
#if 0
#endif
#if 0
Boolean type: The type returned by OpTypeBool.
Integer type: Any width signed or unsigned type from OpTypeInt. By convention, the lowest-order bit will be referred to as bit-number 0, and the highest-order bit as bit-number Width - 1.
Floating-point type: Any width type from OpTypeFloat.
Numerical type: An integer type or a floating-point type.
Scalar: A single instance of a numerical type or Boolean type. Scalars will also be called components when being discussed either by themselves or in the context of the contents of a vector.
Vector: An ordered homogeneous collection of two or more scalars. Vector sizes are quite restrictive and dependent on the execution model.
Matrix: An ordered homogeneous collection of vectors. When vectors are part of a matrix, they will also be called columns. Matrix sizes are quite restrictive and dependent on the execution model.
Array: An ordered homogeneous collection of any non-void-type objects. When an object is part of an array, it will also be called an element. Array sizes are generally not restricted.
Structure: An ordered heterogeneous collection of any non-void types. When an object is part of a structure, it will also be called a member.
Aggregate: A structure or an array.
Composite: An aggregate, a matrix, or a vector.
Image: A traditional texture or image; SPIR-V has this single name for these. An image type is declared with OpTypeImage. An image does not include any information about how to access, filter, or sample it.
Sampler: Settings that describe how to access, filter, or sample an image. Can come either from literal declarations of settings or be an opaque reference to externally bound settings. A sampler does not include an image.
Sampled Image: An image combined with a sampler, enabling filtered accesses of the image’s contents.
Concrete Type: A numerical scalar, vector, or matrix type, or OpTypePointer when using a Physical addressing model, or any aggregate containing only these types.
Abstract Type: An OpTypeVoid or OpTypeBool, or OpTypePointer when using the Logical addressing model, or any aggregate type containing any of these.
Opaque Type: A type that is, or contains, or points to, or contains pointers to, any of the following types:
    OpTypeImage
    OpTypeSampler
    OpTypeSampledImage
    OpTypeOpaque
    OpTypeEvent
    OpTypeDeviceEvent
    OpTypeReserveId
    OpTypeQueue
    OpTypePipe
    OpTypeForwardPointer
#endif










```
-----------------------------------------------------------------------------

module Language.SPIRV
  module Language.SPIRV.Types
  module Language.SPIRV.Binary
  module Language.SPIRV.Pretty
  module Language.SPIRV.Analyze
  module Language.SPIRV.Layout
  module Language.SPIRV.Build
  module Language.SPIRV.Transform

-----------------------------------------------------------------------------

module Language.SPIRV.Config
  module Language.SPIRV.Config.Decode
  module Language.SPIRV.Config.Grammar

module Language.SPIRV.Config.Decode
  configDecode :: Decode

module Language.SPIRV.Config.Grammar
  configInstGrammar :: InstGrammar
  configExtInstGrammars :: Map Name ExtInstGrammar

-----------------------------------------------------------------------------

module Language.SPIRV.Binary
  encode :: Module -> L.ByteString
  decode :: L.ByteString -> Module
  decodeWith :: Decode -> L.ByteString -> Module

module Language.SPIRV.Pretty
  pretty :: Module -> Doc

-----------------------------------------------------------------------------

module Language.SPIRV.Layout
  layout :: Context -> Result Module

module Language.SPIRV.Analyze
  analyze :: Module -> Result Context

module Language.SPIRV.Build
  build :: Header -> Build a -> (a, Context)
  type Build a

module Language.SPIRV.Transform
  type Transform = Module -> Result Module
  renumberIdsFrom :: WORD -> Transform

-----------------------------------------------------------------------------

module Language.SPIRV.Util.Graph
  type Graph
  domSort :: Graph -> [Int]
  topoScc :: Graph -> [IntSet]
  -- ...

-----------------------------------------------------------------------------

module Language.SPIRV.Types
  module Language.SPIRV.Types.Common
  module Language.SPIRV.Types.Context
  module Language.SPIRV.Types.Generic
  module Language.SPIRV.Types.Grammar
  module Language.SPIRV.Types.Core

module Language.SPIRV.Types.Internal
  -- | Lookup tables to decode binary SPIR-V.
  data Decode = Decode
    {decodeInst :: InstDecode
    ,decodeExtInst :: Map Name ExtInstDecode} __D
  data InstDecode = InstDecode
    {instDecodeInst :: IntMap [OperandEncoding]
    ,instDecodeInstIdResult :: IntMap [Int]
    ,instDecodeInstIdResultType :: IntMap [Int]
    ,instDecodeValEnum :: IntMap (IntMap [OperandEncoding])
    ,instDecodeBitEnum :: IntMap [(WORD, [OperandEncoding])]} __D
  data ExtInstDecode = ExtInstDecode
    {extInstDecodeInst :: IntMap [OperandEncoding]} __D
  data OperandEncoding
    = ID
    | LIT_1
    | LIT_N
    | LIT_S
    | ENUM_VAL !Int
    | ENUM_BIT !Int
    | INST
    | TUPLE [OperandEncoding]
    | MAYBE OperandEncoding
    | LIST OperandEncoding __D

module Language.SPIRV.Types.Common
  type WORD
  type STRING
  toString :: STRING -> String
  type Name = STRING
  type Error = STRING
  type Errors = [STRING]
  type Result a = Either Errors a

module Language.SPIRV.Types.Context
  type Context

module Language.SPIRV.Types.Generic
  data Module = Module
    {moduleHeader :: Header
    ,moduleGlobal :: [Instruction]
    ,moduleFunctions :: [Function]} __D
  data Header = Header
    {headerMagic :: WORD
    ,headerVersion :: WORD
    ,headerGenerator :: WORD
    ,headerIdBound :: WORD
    ,headerSchema :: WORD} __D
  data Function = Function
    {functionOp :: Instruction
    ,functionParams :: [Instruction]
    ,functionBlocks :: [Block]} __D
  data Block = Block
    {blockLabel :: Id
    ,blockSuccs :: [Id]
    ,blockInsts :: [Instruction]} __D
  type Instruction = ValEnum
  type ValEnum = Enumerant
  type BitEnum = [Enumerant]
  type Enumerant = (WORD, [Operand])
  data Operand
    = IdO !Id
    | LitO !Lit
    | ValEnumO ValEnum
    | BitEnumO BitEnum
    | InstO Instruction __D
  newtype Id = Id WORD __D
  data Lit = Lit1 !WORD | LitN [WORD] | LitS STRING __D
  data NumLit = Num1L !WORD | NumNL [WORD] __D
  class TagOf a tag where
    tagOf :: a -> tag
  class OperandsOf a where
    operandsOf :: a -> [Operand]
  class IsOperand a where
    toOperand :: a -> Operand
    fromOperand :: Operand -> Maybe a
  class IsLit a where
    toLit :: a -> Lit
    fromLit :: Lit -> Maybe a
  instance TagOf ValEnum WORD where
  instance TagOf BitEnum WORD where
  instance OperandsOf Operand where
  instance OperandsOf ValEnum where
  instance OperandsOf BitEnum where
  instance IsOperand Id where
  instance IsOperand ValEnum where
  instance IsOperand BitEnum where
  instance IsOperand Lit where
  instance IsOperand WORD where
  instance IsOperand [WORD] where
  instance IsOperand STRING where
  instance IsOperand NumLit where
  instance IsLit WORD where
  instance IsLit [WORD] where
  instance IsLit STRING where
  instance IsLit NumLit where
  instance IsString Operand where
  instance IsString Lit where

module Language.SPIRV.Types.Decode
  type Decode

module Language.SPIRV.Types.Grammar
  -- | Build the @Decode@ datastructure from grammar.
  grammarToDecode :: InstGrammar -> Map Name ExtInstGrammar -> Decode
  -- | Specifies the core SPIR-V instructions and operands.
  data InstGrammar = InstGrammar
    {instGrammarMagicNumber :: WORD --"0x07230203",
    ,instGrammarMajorVersion :: Word8
    ,instGrammarMinorVersion :: Word8
    ,instGrammarRevision :: WORD
    ,instGrammarInsts :: [InstSpec]
    ,instGrammarOperands :: [OperandSpec]} __D
  -- | Specified an external instruction set.
  data ExtInstGrammar = ExtInstGrammar
    {extInstGrammarVersion :: WORD
    ,extInstGrammarRevision :: WORD
    ,extInstGrammarInsts :: [InstSpec]} __D
  -- | Specifiies a single instruction.
  data InstSpec = InstSpec
    {instSpecName :: Name
    ,instSpecOpCode :: WORD
    ,instSpecIdResultIxs :: [Int]
    ,instSpecIdResultTypeIxs :: [Int]
    ,instSpecOperands :: [OperandEncoding]
    ,instSpecCapabilities :: [STRING]} __D
  -- | Specifies an operand type.
  data OperandSpec
    = BasicOperandSpec Name X.OperandEncoding
    | CompositeOperandSpec Name [OperandEncoding]
    | ValEnumOperandSpec Name [(Name, WORD, [OperandEncoding], [STRING])]
    | BitEnumOperandSpec Name [(Name, WORD, [OperandEncoding], [STRING])] __D
  -- | Specifies the encoding of an operand field in either an instruction
  -- or an operand type.
  data OperandEncoding
    = ONE Name
    | MAYBE OperandEncoding
    | LIST OperandEncoding __D
  instance Ppr OperandEncoding where

module Language.SPIRV.Types.Core
  class Empty a where
    empty :: a
    isEmpty :: a -> Bool
  class IsValEnum a where
    valEnumKey :: a -> ValEnumKey
    toValEnum :: a -> ValEnum
    fromValEnum :: ValEnum -> Maybe a
  class IsBitEnum a where
    bitEnumKey :: a -> BitEnumKey
    toBitEnum :: a -> BitEnum
    fromBitEnum :: BitEnum -> Maybe a
  class IsOperands a where
    toOperands :: a -> [Operand]
    fromOperands :: [Operand] -> Maybe a
  data ValEnumKey
    = OpVEK
    | CapabilityVEK
    | SourceLanguageVEK
    | ExecutionModelVEK
    | AddressingModelVEK
    | MemoryModelVEK
    | ExecutionModeVEK
    | StorageClassVEK
    | DimVEK
    | SamplerAddressingModeVEK
    | SamplerFilterModeVEK
    | ImageFormatVEK
    | ImageChannelOrderVEK
    | ImageChannelDataTypeVEK
    | FPRoundingModeVEK
    | LinkageTypeVEK
    | AccessQualifierVEK
    | FunctionParameterAttributeVEK
    | DecorationVEK
    | BuiltInVEK
    | ScopeVEK
    | GroupOperationVEK
    | KernelEnqueueFlagsVEK __D_BE
  data BitEnumKey
    = ImageOperandsBEK
    | FPFastMathModeBEK
    | SelectionControlBEK
    | LoopControlBEK
    | FunctionControlBEK
    | MemorySemanticsBEK
    | MemoryAccessBEK
    | KernelProfilingInfoBEK __D_BE
  type LiteralInteger = WORD
  type LiteralString = STRING
  type LiteralContextDependentNumber = NumLit
  type IdRef = Id
  type IdResult = Id
  type IdResultType = Id
  type IdScope = Id
  type IdMemorySemantics = Id
  type IdFunctionType = Id
  type IdLabel = Id
  type LiteralSpecConstantOpInteger = LiteralInteger
  type LiteralExtInstInteger = LiteralInteger
  type PairIdRefIdRef = (IdRef, IdRef)
  type PairIdRefLiteralInteger = (IdRef, LiteralInteger)
  type PairLiteralIntegerIdRef = (LiteralInteger, IdRef)
  data Op = ..
  data ImageOperands = ..
  data FPFastMathMode = ..
  data SelectionControl = ..
  data LoopControl = ..
  data FunctionControl = ..
  data MemorySemantics = ..
  data MemoryAccess = ..
  data KernelProfilingInfo = ..
  data ExecutionMode = ..
  data Decoration = ..
  data Capability = ..
  data SourceLanguage = ..
  data ExecutionModel = ..
  data AddressingModel = ..
  data MemoryModel = ..
  data StorageClass = ..
  data Dim = ..
  data SamplerAddressingMode = ..
  data SamplerFilterMode = ..
  data ImageFormat = ..
  data ImageChannelOrder = ..
  data ImageChannelDataType = ..
  data FPRoundingMode = ..
  data LinkageType = ..
  data AccessQualifier = ..
  data FunctionParameterAttribute = ..
  data BuiltIn = ..
  data Scope = ..
  data GroupOperation = ..
  data KernelEnqueueFlags = ..
  instance Empty FPFastMathMode
  instance Empty FunctionControl
  instance Empty ImageOperands
  instance Empty KernelProfilingInfo
  instance Empty LoopControl
  instance Empty MemoryAccess
  instance Empty MemorySemantics
  instance Empty SelectionControl
  instance IsBitEnum FPFastMathMode
  instance IsBitEnum FunctionControl
  instance IsBitEnum ImageOperands
  instance IsBitEnum KernelProfilingInfo
  instance IsBitEnum LoopControl
  instance IsBitEnum MemoryAccess
  instance IsBitEnum MemorySemantics
  instance IsBitEnum SelectionControl
  instance IsOperand AccessQualifier
  instance IsOperand AddressingModel
  instance (IsOperand a, IsOperand b) => IsOperands (a, b)
  instance (IsOperand a, IsOperand b) => IsOperands [(a, b)]
  instance (IsOperand a) => IsOperands (Maybe a)
  instance IsOperand BuiltIn
  instance IsOperand Capability
  instance IsOperand Decoration
  instance IsOperand Dim
  instance IsOperand ExecutionMode
  instance IsOperand ExecutionModel
  instance IsOperand FPFastMathMode
  instance IsOperand FPRoundingMode
  instance IsOperand FunctionControl
  instance IsOperand FunctionParameterAttribute
  instance IsOperand GroupOperation
  instance IsOperand ImageChannelDataType
  instance IsOperand ImageChannelOrder
  instance IsOperand ImageFormat
  instance IsOperand ImageOperands
  instance IsOperand KernelEnqueueFlags
  instance IsOperand KernelProfilingInfo
  instance IsOperand LinkageType
  instance IsOperand LoopControl
  instance IsOperand MemoryAccess
  instance IsOperand MemoryModel
  instance IsOperand MemorySemantics
  instance IsOperand Op
  instance IsOperands AccessQualifier
  instance IsOperands AddressingModel
  instance IsOperand SamplerAddressingMode
  instance IsOperand SamplerFilterMode
  instance IsOperands BitEnum
  instance IsOperands BuiltIn
  instance IsOperands Capability
  instance IsOperand Scope
  instance IsOperands Decoration
  instance IsOperands Dim
  instance IsOperand SelectionControl
  instance IsOperands ExecutionMode
  instance IsOperands ExecutionModel
  instance IsOperands FPFastMathMode
  instance IsOperands FPRoundingMode
  instance IsOperands FunctionControl
  instance IsOperands FunctionParameterAttribute
  instance IsOperands GroupOperation
  instance IsOperands Id
  instance IsOperands [Id]
  instance IsOperands ImageChannelDataType
  instance IsOperands ImageChannelOrder
  instance IsOperands ImageFormat
  instance IsOperands ImageOperands
  instance IsOperands KernelEnqueueFlags
  instance IsOperands KernelProfilingInfo
  instance IsOperands LinkageType
  instance IsOperands Lit
  instance IsOperands LoopControl
  instance IsOperands MemoryAccess
  instance IsOperands MemoryModel
  instance IsOperands MemorySemantics
  instance IsOperands NumLit
  instance IsOperands Op
  instance IsOperand SourceLanguage
  instance IsOperands SamplerAddressingMode
  instance IsOperands SamplerFilterMode
  instance IsOperands Scope
  instance IsOperands SelectionControl
  instance IsOperands SourceLanguage
  instance IsOperands StorageClass
  instance IsOperands STRING
  instance IsOperand StorageClass
  instance IsOperands ValEnum
  instance IsOperands WORD
  instance IsOperands [WORD]
  instance IsValEnum AccessQualifier
  instance IsValEnum AddressingModel
  instance IsValEnum BuiltIn
  instance IsValEnum Capability
  instance IsValEnum Decoration
  instance IsValEnum Dim
  instance IsValEnum ExecutionMode
  instance IsValEnum ExecutionModel
  instance IsValEnum FPRoundingMode
  instance IsValEnum FunctionParameterAttribute
  instance IsValEnum GroupOperation
  instance IsValEnum ImageChannelDataType
  instance IsValEnum ImageChannelOrder
  instance IsValEnum ImageFormat
  instance IsValEnum KernelEnqueueFlags
  instance IsValEnum LinkageType
  instance IsValEnum MemoryModel
  instance IsValEnum Op
  instance IsValEnum SamplerAddressingMode
  instance IsValEnum SamplerFilterMode
  instance IsValEnum Scope
  instance IsValEnum SourceLanguage
  instance IsValEnum StorageClass



-----------------------------------------------------------------------------
```





## XXX

```
signature PARTREFINE = sig
  type PartRefine
  type DeltaInv = EdgeLblMap (NodeMap NodeSet)
  partRefineInit :: Partition -> DeltaInv -> PartRefine
  partRefineRefine :: PartRefine -> PartRefine
  partRefinePartition :: PartRefine -> Partition
  partRefineDeltaInv :: PartRefine -> DeltaInv

structure PartRefine :: PARTREFINE = struct
  data PartRefine = PartRefine
    {partRefinePart :: PartRefinePart
    ,partRefineDeltaInv :: DeltaInv}
  type EdgeLbl = Int
  type EdgeLblMap = IntMap
  type DeltaInv = EdgeLblMap (NodeMap NodeSet)
  type PartRefineSize = Int
  data PartRefinePart = PRP
    {prpNext   :: !Node
    ,prpC2Size :: NodeMap PartRefineSize
    ,prpN2C    :: NodeMap Int
    ,prpClass  :: IntMap NodeSet}
  partRefineInit :: Partition -> DeltaInv -> PartRefine
  partRefineInit (n2i,i2ns) dinv
    | IM.null n2i || IM.null i2ns || IM.null dinv
    = PartRefine
        {partRefinePart=PRP 0 mempty mempty mempty
        ,partRefineDeltaInv=mempty}
    | prpNext <- 1 + maxKey i2ns
    , prpC2Size <- fmap IM.size i2ns
    , prpN2C <- n2i
    , prpClass <- i2ns
    , partRefinePart <- PRP{..}
    , partRefineDeltaInv <- dinv
    = PartRefine{..}
    where maxKey m
            | Just ((k,_),_) <- IM.maxViewWithKey m = k
            | otherwise = __IMPOSSIBLE
  -- | Hopcroft's Partition-Refinement Algorithm
  partRefineRefine :: PartRefine -> PartRefine
  partRefineRefine PartRefine{..}
    | partRefinePart <- hopcroft partRefineDeltaInv partRefinePart
    = PartRefine{..}
    where hopcroft :: DeltaInv -> PartRefinePart -> PartRefinePart
          hopcroft dinv part = go (part, toSets part)
            where elbls = edgeLbls dinv
                  go :: PartRefineStepState -> PartRefinePart
                  go (ps,[]) = ps
                  go (ps,l:ls) = go (fold l (ps,ls))
                  fold l s = foldl' (\s elbl->
                    partRefineStep s (deltaInv dinv elbl l)
                    ) s elbls
                  toSets :: PartRefinePart -> [NodeSet]
                  toSets PRP{..} = IM.elems prpClass
                  edgeLbls :: DeltaInv -> [EdgeLbl]
                  edgeLbls = IM.keys
                  deltaInv :: DeltaInv -> EdgeLbl -> NodeSet -> NodeSet
                  deltaInv dinv e ns = IM.fold (\/) mempty
                    ((dinv IM.! e) `IM.intersection` ns)
  type PartRefineStepState = (PartRefinePart, [NodeSet])
  partRefineStep :: PartRefineStepState -> NodeSet -> PartRefineStepState
  partRefineStep s a = go s a
    where go s@(PRP{prpN2C},_) a
            | IM.null a = s
            | i <- minKey a
            , cls <- prpN2C IM.! i
            , (snew, anew) <- refineOne s cls a
            = go snew anew
          minKey m
            | Just ((k,_),_) <- IM.minViewWithKey m = k
            | otherwise = __IMPOSSIBLE
          refineOne
            :: PartRefineStepState -> Int
            -> NodeSet -> (PartRefineStepState, NodeSet)
          refineOne s@(part@PRP{prpClass},ls) cls dinv
            | p <- prpClass IM.! cls
            , p1 <- p/\dinv
            , p2 <- p\\dinv
            , xdinv <- dinv\\p
            , o1 <- IM.null p1
            , o2 <- IM.null p2
            = case (o1,o2) of
                (True,True)-> __IMPOSSIBLE
                (True,False) | __ASSERT(p == p2)-> (s, xdinv)
                (False,True) | __ASSERT(p == p1)-> (s, xdinv)
                (False,False)
                  | (part, p0) <- split part cls p1 p2
                  -> ((part, p0:ls), xdinv)
          -- Splits the smaller of the two sets into a new class, and
          -- returns the smaller one. It MUST be the case and is UNCHECKED
          -- that the two sets are NONMEMPTY. And it MUST be the case and
          -- is UNCHECKED that the two sets form a partition of the class
          -- identified by the @Ix CLASS@.
          split
            :: PartRefinePart -> Int
            -> NodeSet -> NodeSet
            -> (PartRefinePart, NodeSet)
          split PRP{..} cls p1 p2
            | n1 <- IM.size p1 -- XXX: O(n)
            , n2 <- (prpC2Size IM.! cls) - n1
            , let go x1 x2 m1 m2
                    | !new <- prpNext
                    , !prpNext <- prpNext + 1
                    , !prpN2C <- fmap (const new) x2`IM.intersection`prpN2C
                    , !prpC2Size <- IM.insert cls m1 prpC2Size
                    , !prpC2Size <- IM.insert new m2 prpC2Size
                    , !prpClass <- IM.insert cls x1 prpClass
                    , !prpClass <- IM.insert new x2 prpClass
                    = PRP{..}
            = case n1 <= n2 of
                True  | !out <- go p1 p2 n1 n2-> (out, p1)
                False | !out <- go p2 p1 n2 n1-> (out, p2)
```



