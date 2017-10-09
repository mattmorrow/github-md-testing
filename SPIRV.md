# SPIR-V

## Id Kinds

```haskell
data IdKind
  = IDK_X     -- SSA Name
  | IDK_VL    -- OpVariable (Local)
  | IDK_B     -- OpLabel
  | IDK_FP    -- OpFunctionParameter
  | IDK_F     -- OpFunction
  | IDK_VG    -- OpVariable (Global)
  | IDK_C     -- Constant
  | IDK_U     -- OpUndef
  | IDK_T     -- Type
  | IDK_DG    -- OpDecorationGroup
  | IDK_S     -- OpString
  | IDK_EII   -- OpExtInstImport
```

### Id Kind constraints on IdRefs

* IDK_EII
    * Defined by:
        * OpExtInstImport
    * Used by:
        * OpExtInst
* IDK_S
    * Defined by:
        * OpString
    * Used by:
        * OpSource
        * OpLine
* IDK_DG
    * Defined by:
        * OpDecorationGroup
    * Used by:
        * OpGroupDecorate[0]
        * OpGroupMemberDecorate[0]
        * OpDecorate[0]
        * OpDecorateId[0]
        * OpMemberDecorate[0]
* IDK_T
    * Forward declared by:
        * OpTypeForwardPointer
            * OpTypePointer
    * Defined by:
        * OpTypeVoid
        * OpTypeBool
        * OpTypeInt
        * OpTypeFloat
        * OpTypeVector
        * OpTypeMatrix
        * OpTypeImage
        * OpTypeSampler
        * OpTypeSampledImage
        * OpTypeArray
        * OpTypeRuntimeArray
        * OpTypeStruct
        * OpTypeOpaque
        * OpTypePointer
        * OpTypeFunction
        * OpTypeEvent
        * OpTypeDeviceEvent
        * OpTypeReserveId
        * OpTypeQueue
        * OpTypePipe
        * OpTypePipeStorage
        * OpTypeNamedBarrier
    * Used by:
        * OpTypeForwardPointer
        * OpTypeVector
        * OpTypeMatrix
        * OpTypeImage
        * OpTypeSampledImage
        * OpTypeArray
        * OpTypeRuntimeArray
        * OpTypeStruct
        * OpTypePointer
        * OpTypeFunction
        * <code-insns>
        * <decoration-insns>
* IDK_C
    * Defined by:
        * OpConstantTrue
        * OpConstantFalse
        * OpConstant
        * OpConstantComposite
        * OpConstantSampler
        * OpConstantNull
        * OpSpecConstantTrue
        * OpSpecConstantFalse
        * OpSpecConstant
        * OpSpecConstantComposite
        * OpSpecConstantOp
        * OpConstantPipeStorage
    * Used by:
        * OpVariable<not(StorageClassFunction)>
        * OpVariable<StorageClassFunction>
        * OpTypeArray
            * OpConstant
            * OpSpecConstant
            * OpSpecConstantOp
        * OpConstantComposite
        * OpSpecConstantComposite
        * OpSpecConstantOp
        * <decoration-insns>
* IDK_U
    * Defined by:
        * OpUndef
    * Used by:
        * OpVariable<not(StorageClassFunction)>
        * OpVariable<StorageClassFunction>
        * OpConstantComposite
        * OpSpecConstantComposite
        * OpSpecConstantOp
        * <code-insns>
        * <decoration-insns>
* IDK_VG
    * Defined by:
        * OpVariable<not(StorageClassFunction)>
    * Used by:
        * OpVariable<not(StorageClassFunction)>
        * OpVariable<StorageClassFunction>
        * OpAccessChain
        * OpInBoundsAccessChain
        * OpPtrAccessChain
        * OpInBoundsPtrAccessChain
        * <decoration-insns>
* IDK_VL
    * Defined by:
        * OpVariable<StorageClassFunction>
    * Used by:
        * OpVariable<StorageClassFunction>
        * OpAccessChain
        * OpInBoundsAccessChain
        * OpPtrAccessChain
        * OpInBoundsPtrAccessChain
        * <decoration-insns>
* IDK_F
    * Defined by:
        * OpFunction
    * Used by:
        * OpFunctionCall
        * OpEntryPoint
        * OpExecutionMode
        * OpExecutionModeId
        * <decoration-insns>
* IDK_FP
    * Defined by:
        * OpFunctionParameter
    * Used by:
        * <code-insns>
        * <decoration-insns>
* IDK_B
    * ALWAYS HAS ASSOCIATED
        * Ix FUNCTION
    * Defined by:
        * OpLabel
    * Used by:
        * OpPhi
        * OpSwitch
        * OpBranch
        * OpBranchConditional
        * OpLoopMerge
        * OpSelectionMerge
        * <decoration-insns>
* IDK_X
    * ALWAYS HAS ASSOCIATED:
        * Ix TYPE
        * Ix BLOCK
    * Defined by:
        * <code-insns-with-idresult>
    * Used by:
        * <code-insns>
        * <decoration-insns>

## FunctionParameterAttribute, LinkageAttributes, FPRoundingMode, FPFastMathMode

1) Zext
2) Sext
3) ByVal
4) Sret
5) NoAlias
6) NoCapture
7) NoWrite
8) NoReadWrite

1) Export
2) Import

1) RTE
2) RTZ
3) RTP
4) RTN

1) NotNaN      ::  Bool
1) NotInf      ::  Bool
1) NSZ         ::  Bool
1) AllowRecip  ::  Bool
1) Fast        ::  Bool

## Op{Decorate{,Id},MemberDecorate} IdRef IdKind constraints given Decoration:

`OpDecorate(Id,Decoration)`
`OpDecorateId(Id,Decoration)`
`OpMemberDecorate((Id,WORD),Decoration)`

|X|Y|
|---|---|
|IDK_EII|<NONE>|
|IDK_S|<NONE>|
|IDK_DG|<ANY>|
|IDK_FP|FuncParamAttr|
|IDK_VG|BuiltIn|
|IDK_T(mbr(struct))|BuiltIn|
|IDK_VG|LinkageAttributes|
|IDK_F|LinkageAttributes|
|IDK_C(spec)|SpecId|
|IDK_T(struct)|CPacked|
|IDK_F|FPRoundingMode|
|IDK_FP|FPRoundingMode|
|IDK_X|FPRoundingMode|
|IDK_F|FPFastMathMode|
|IDK_FP|FPFastMathMode|
|IDK_X|FPFastMathMode|
|IDK_VG|Restrict|
|IDK_VL|Restrict|
|IDK_FP|Restrict|
|IDK_X(ptr)|Restrict|
|IDK_VG|Aliased|
|IDK_VL|Aliased|
|IDK_FP|Aliased|
|IDK_X(ptr)|Aliased|
|IDK_VG|Volatile|
|IDK_VL|Volatile|
|IDK_FP|Volatile|
|IDK_X(ptr)|Volatile|
|IDK_VG|Constant|
|IDK_VL|Constant|
|IDK_FP|Constant|
|IDK_X(ptr)|Constant|
|IDK_VG|Coherent|
|IDK_VL|Coherent|
|IDK_FP|Coherent|
|IDK_X(ptr)|Coherent|
|IDK_VG|NonWritable|
|IDK_VL|NonWritable|
|IDK_FP|NonWritable|
|IDK_X(ptr)|NonWritable|
|IDK_VG|NonReadable|
|IDK_VL|NonReadable|
|IDK_FP|NonReadable|
|IDK_X(ptr)|NonReadable|
|IDK_X|RelaxedPrecision|
|IDK_T(struct)|Block|
|IDK_T(mbr(struct))|RowMajor|
|IDK_T(mbr(struct))|ColMajor|
|IDK_T(array)|ArrayStride|
|IDK_T(mbr(struct))|MatrixStride|
|IDK_T(struct)|GLSLShared|
|IDK_T(struct)|GLSLPacked|
|<obj>|Uniform|
|IDK_T(mbr(struct))|Uniform|
|IDK_X(cvt(_,int))|SaturatedConversion|
|IDK_X(ptr)|Alignment|
|IDK_X(ptr)|AlignmentId|
|IDK_X(ptr)|MaxByteOffset|
|IDK_X(ptr)|MaxByteOffsetId|
|<TODO>|NoContraction|
|<TODO>|BufferBlock|
|<TODO>|NoPerspective|
|<TODO>|Flat|
|<TODO>|Patch|
|<TODO>|Centroid|
|<TODO>|Sample|
|<TODO>|Invariant|
|<TODO>|Stream|
|<TODO>|Location|
|<TODO>|Component|
|<TODO>|Index|
|<TODO>|Binding|
|<TODO>|DescriptorSet|
|<TODO>|Offset|
|<TODO>|XfbBuffer|
|<TODO>|XfbStride|
|<TODO>|InputAttachmentIndex|
|<TODO>|ExplicitInterpAMD|
|<TODO>|OverrideCoverageNV|
|<TODO>|PassthroughNV|
|<TODO>|ViewportRelativeNV|
|<TODO>|SecondaryViewportRelativeNV|

## NOTES

* 15404 OpFunction Result <id> can only be used by OpFunctionCall, OpEntryPoint, and decoration instructions.
* 14690 OpSwitch literal width (and hence number of operands) is determined by the type of Selector, and be rigorous about how sub-32-bit literals are stored.
* RelaxedPrecision Decoration can be applied to OpFunction (14662).
* 14325 mutual exclusion validation rules of Execution Modes and Decorations
* Float16 has no dependencies (15234).
* Offset Decoration should only be for Shader (15268).
* Generic Storage Class is supposed to need the GenericPointer Capability (14287).
* Remove capability restriction on the BuiltIn Decoration (15248).
* 15225 Include no re-association as a constraint required by the NoContraction Decoration.
* 15210 Clarify OpPhi semantics that operand values only come from parent blocks.

