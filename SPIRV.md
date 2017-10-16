# SPIR-V

## RULES

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
– 15459 Clarify what makes different aggregate types in "Types and Variables".
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
The OpSizeOf instruction.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
The Initializer and Finalizer and Execution Modes.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

A.7
Changes from Version 1.00, Revision 5
• Khronos SPIR-V issue #27: Removed Shader dependency from SampledBuffer
  and Sampled1D Capabilities.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
• Khronos SPIR-V issue #56: Clarify that the meaning of "read-only"
  in the Storage Classes includes not allowing initializers.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
• Khronos SPIR-V issue #57: Clarify "modulo" means "remainder" in
  OpFMod’s description.
• Khronos SPIR-V issue #60: OpControlBarrier synchronizes Output
  variables when used in tessellation-control shader.
• Public SPIRV-Headers issue #1: Remove the Shader capability
  requirement from the Input Storage Class.
• Public SPIRV-Headers issue #10: Don’t say the (u [, v] [, w], q)
  has four components, as it can be closed up when the optional ones are
  missing. Seen in the projective image instructions.
• Public SPIRV-Headers issues #14: add Max enumerants of 0x7FFFFFFF
  to each of the non-mask enums in the C-based header files.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
• Public SPIRV-Headers issues #12 and #13 and Khronos SPIR-V issue #65:
  Allow OpVariable as an initializer for another OpVariable instruction
  or the Base of an OpSpecConstantOp with an AccessChain opcode.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

A.12
Changes from Version 1.00, Revision 10
• Add HLSL source language.
• Add StorageBuffer storage class.
• Add StorageBuffer16BitAccess, UniformAndStorageBuffer16BitAccess,
  VariablePointersStorageBuffer, and VariablePointers capabilities.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
• Khronos SPIR-V issue #163: Be more clear that OpTypeStruct allows zero
  members. Also affects ArrayStride and Offset decoration validation rules.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
• Khronos SPIR-V issue #159: List allowed AtomicCounter instructions with the AtomicStorage capability rather than the validation rules.
• Khronos SPIR-V issue #36: Describe more clearly the type of ND Range in OpGetKernelNDrangeSubGroupCount, OpGetKernelNDrangeMaxSubGroupSize, and OpEnqueueKernel.
• Khronos SPIR-V issue #128: Be clear the OpDot operates only on vectors.
• Khronos SPIR-V issue #80: Loop headers must dominate their continue target. See Structured Control Flow.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
• Khronos SPIR-V issue #150 allow UniformConstant storage-class
  variables to have initializers, depending on the client API.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

A.13
Changes from Version 1.00
• Moved version number to SPIR-V 1.1
• New functionality:
    – Bug 14202 named barriers:
        * Added the NamedBarrier Capability.
        * Added the instructions: OpTypeNamedBarrier, OpNamedBarrierInitialize, and OpMemoryNamedBarrier.
    – Bug 14201 subgroup dispatch:
        * Added the SubgroupDispatch Capability.
        * Added the instructions: OpGetKernelLocalSizeForSubgroupCount and OpGetKernelMaxNumSubgroups.
        * Added SubgroupSize and SubgroupsPerWorkgroup Execution Modes.
    – Bug 14441 program-scope pipes:
        * Added the PipeStorage Capability.
        * Added Instructions: OpTypePipeStorage, OpConstantPipeStorage, and OpCreatePipeFromPipeStorage.
    – Bug 15434 Added the OpSizeOf instruction.
    – Bug 15024 support for OpenCL-C++ ivdep loop attribute:
        * Added DependencyInfinite and DependencyLength Loop Controls.
        * Updated OpLoopMerge to support these.
    - Bug 14022 Added Initializer and Finalizer and Execution Modes.
    - Bug 15539 Added the MaxByteOffset Decoration.
    - Bug 15073 Added the Kernel Capability to the SpecId Decoration.
    - Bug 14828 Added the OpModuleProcessed instruction.
• Fixed internal bugs:
    – Bug 15481 Clarification on alignment and size operands for pipe operands

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

## `Decoration`

* CONSIDERATIONS
    1) `allowed :: Map IdKind [DecorationTAG]`
    2) `semantics :: Map IdKind XXX`, where `XXX =`
        1) Regarding EQUALITY of entities:
            1) `mustBeExactlyEqual :: [DecorationTAG]`
            2) `dontMatterForEquality := allDecorationTags\\mustBeExactlyEqual`

* `Decoration`
    1) `DecorationFuncParamAttr FunctionParameterAttribute`
        1) `Zext`
        2) `Sext`
        3) `ByVal`
        4) `Sret`
        5) `NoAlias`
        6) `NoCapture`
        7) `NoWrite`
        8) `NoReadWrite`
    2) `DecorationLinkageAttributes STRING LinkageType`
        1) `Export`
        2) `Import`
    3) `DecorationFPRoundingMode FPRoundingMode`
        1) `RTE`
        2) `RTZ`
        3) `RTP`
        4) `RTN`
    4) `DecorationFPFastMathMode FPFastMathMode`
        1) FPFastMathMode
            1) `NotNaN      ::  Bool`
            2) `NotInf      ::  Bool`
            3) `NSZ         ::  Bool`
            4) `AllowRecip  ::  Bool`
            5) `Fast        ::  Bool`
    5) `DecorationBuiltIn BuiltIn`
            1) ...
            2) ...
            3) ...
    6) DecorationSpecId WORD
    7) Decorations with embedded `Id`s:
        1) `DecorationAlignmentId Id`
        2) `DecorationMaxByteOffsetId Id`
    8) .
        * `DecorationAlignment WORD`
        * `DecorationMaxByteOffset WORD`
        * `DecorationAlignmentId Id`
        * `DecorationMaxByteOffsetId Id`
    9) Type Layout Specification
        * `CapabilityKernel`
            * `DecorationCPacked`
        * `CapabilityShader`
            * `DecorationGLSLPacked`
            * `DecorationGLSLShared`

TOORG:

```haskell
DecorationRelaxedPrecision
DecorationSaturatedConversion
DecorationBlock
DecorationBufferBlock
DecorationRowMajor
DecorationColMajor
DecorationArrayStride                  WORD
DecorationMatrixStride                 WORD
DecorationNoPerspective
DecorationFlat
DecorationPatch
DecorationCentroid
DecorationSample
DecorationInvariant
DecorationRestrict
DecorationAliased
DecorationVolatile
DecorationConstant
DecorationCoherent
DecorationNonWritable
DecorationNonReadable
DecorationUniform
DecorationStream                       WORD
DecorationLocation                     WORD
DecorationComponent                    WORD
DecorationIndex                        WORD
DecorationBinding                      WORD
DecorationDescriptorSet                WORD
DecorationOffset                       WORD
DecorationXfbBuffer                    WORD
DecorationXfbStride                    WORD
DecorationNoContraction
DecorationInputAttachmentIndex         WORD
DecorationExplicitInterpAMD
DecorationOverrideCoverageNV
DecorationPassthroughNV
DecorationViewportRelativeNV
DecorationSecondaryViewportRelativeNV  WORD
```


* Capability to Decorations possibly allowed:
    * NONE
        * Aliased
        * BuiltIn
        * Coherent
        * ExplicitInterpAMD
        * NonReadable
        * NonWritable
        * Restrict
        * Volatile
    * Kernel
        * Alignment
        * AlignmentId
        * CPacked
        * Constant
        * FPFastMathMode
        * FPRoundingMode
        * FuncParamAttr
        * SaturatedConversion
        * SpecId
    * Shader
        * ArrayStride
        * Binding
        * Block
        * BufferBlock
        * Centroid
        * Component
        * DescriptorSet
        * Flat
        * GLSLPacked
        * GLSLShared
        * Index
        * Invariant
        * Location
        * NoContraction
        * NoPerspective
        * Offset
        * RelaxedPrecision
        * SpecId
        * Uniform
    * Linkage
        * LinkageAttributes
    * Addresses
        * MaxByteOffset
        * MaxByteOffsetId
    * Matrix
        * ColMajor
        * MatrixStride
        * RowMajor
    * GeometryShaderPassthroughNV
        * PassthroughNV
    * GeometryStreams
        * Stream
    * InputAttachment
        * InputAttachmentIndex
    * SampleMaskOverrideCoverageNV
        * OverrideCoverageNV
    * SampleRateShading
        * Sample
    * ShaderStereoViewNV
        * SecondaryViewportRelativeNV
    * ShaderViewportMaskNV
        * ViewportRelativeNV
    * StorageInputOutput16
        * FPRoundingMode
    * StoragePushConstant16
        * FPRoundingMode
    * StorageUniform16
        * FPRoundingMode
    * StorageUniformBufferBlock16
        * FPRoundingMode
    * Tessellation
        * Patch
    * TransformFeedback
        * XfbBuffer
        * XfbStride

```haskell
("RelaxedPrecision", 0, [], ["Shader"]),
("SpecId", 1, [("LiteralInteger")], ["Shader", "Kernel"]),
("Block", 2, [], ["Shader"]),
("BufferBlock", 3, [], ["Shader"]),
("RowMajor", 4, [], ["Matrix"]),
("ColMajor", 5, [], ["Matrix"]),
("ArrayStride", 6, [("LiteralInteger")], ["Shader"]),
("MatrixStride", 7, [("LiteralInteger")], ["Matrix"]),
("GLSLShared", 8, [], ["Shader"]),
("GLSLPacked", 9, [], ["Shader"]),
("CPacked", 10, [], ["Kernel"]),
("BuiltIn", 11, [("BuiltIn")], []),
("NoPerspective", 13, [], ["Shader"]),
("Flat", 14, [], ["Shader"]),
("Patch", 15, [], ["Tessellation"]),
("Centroid", 16, [], ["Shader"]),
("Sample", 17, [], ["SampleRateShading"]),
("Invariant", 18, [], ["Shader"]),
("Restrict", 19, [], []),
("Aliased", 20, [], []),
("Volatile", 21, [], []),
("Constant", 22, [], ["Kernel"]),
("Coherent", 23, [], []),
("NonWritable", 24, [], []),
("NonReadable", 25, [], []),
("Uniform", 26, [], ["Shader"]),
("SaturatedConversion", 28, [], ["Kernel"]),
("Stream", 29, [("LiteralInteger")], ["GeometryStreams"]),
("Location", 30, [("LiteralInteger")], ["Shader"]),
("Component", 31, [("LiteralInteger")], ["Shader"]),
("Index", 32, [("LiteralInteger")], ["Shader"]),
("Binding", 33, [("LiteralInteger")], ["Shader"]),
("DescriptorSet", 34, [("LiteralInteger")], ["Shader"]),
("Offset", 35, [("LiteralInteger")], ["Shader"]),
("XfbBuffer", 36, [("LiteralInteger")], ["TransformFeedback"]),
("XfbStride", 37, [("LiteralInteger")], ["TransformFeedback"]),
("FuncParamAttr", 38, [("FunctionParameterAttribute")], ["Kernel"]),
("FPRoundingMode", 39, [("FPRoundingMode")], ["Kernel", "StorageUniformBufferBlock16", "StorageUniform16", "StoragePushConstant16", "StorageInputOutput16"]),
("FPFastMathMode", 40, [("FPFastMathMode")], ["Kernel"]),
("LinkageAttributes", 41, [("LiteralString"), ("LinkageType")], ["Linkage"]),
("NoContraction", 42, [], ["Shader"]),
("InputAttachmentIndex", 43, [("LiteralInteger")], ["InputAttachment"]),
("Alignment", 44, [("LiteralInteger")], ["Kernel"]),
("MaxByteOffset", 45, [("LiteralInteger")], ["Addresses"]),
("AlignmentId", 46, [("IdRef")], ["Kernel"]),
("MaxByteOffsetId", 47, [("IdRef")], ["Addresses"]),
("ExplicitInterpAMD", 4999, [], []),
("OverrideCoverageNV", 5248, [], ["SampleMaskOverrideCoverageNV"]),
("PassthroughNV", 5250, [], ["GeometryShaderPassthroughNV"]),
("ViewportRelativeNV", 5252, [], ["ShaderViewportMaskNV"]),
("SecondaryViewportRelativeNV", 5256, [("LiteralInteger")], ["ShaderStereoViewNV"])
```

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

