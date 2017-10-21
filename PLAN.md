# The Plan

...

## One

```haskell
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

#if 0
* SUMMARY OF THE TODOS/TODONES THIS FILE REPRESENTS:
    1) [ ] MM.<CODE> <--> SPIRV.<CODE>
        1) .
            * MM.{OpDB,OP,BUILTIN}
            * SPIRV.{Op,EII.{OpenCL,..}}
    2) [/] MM.<ENT> <--> SPIRV.<ENT>
        1) .
            * <<LINKAGE>>
            * <<ENTRYPOINTS>>
            * <<BUILTINS>>
            * DEFN<x> <- _
            * ADDRSPACE <- _
            * ATTRS<__GLOBAL__>
            * MD<__GLOBAL__>
            * ATTRS<x> <- _
            * SUBATTRS<x> <- _
            * MD<x> <- _
            * OP <- f(..,_,..)
            * OPERANDS += _
            * enum int ARG
            * enum int RESULT
        2) .
            * class To, From, AddNumLit
            * data XDecoration, XExecutionMode
#endif

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class To a b where to :: (Id -> NumLit) -> a -> b
class (AddNumLit env) => From env a b where from :: env -> -> b -> (a, env)
class AddNumLit env where 7addNumLit :: env -> NumLit -> (Id, env)

data XDecoration
-- {{{
  = XDecorationRelaxedPrecision
  | XDecorationSpecId                       WORD
  | XDecorationBlock
  | XDecorationBufferBlock
  | XDecorationRowMajor
  | XDecorationColMajor
  | XDecorationArrayStride                  WORD
  | XDecorationMatrixStride                 WORD
  | XDecorationGLSLShared
  | XDecorationGLSLPacked
  | XDecorationCPacked
  | XDecorationBuiltIn                      BuiltIn
  | XDecorationNoPerspective
  | XDecorationFlat
  | XDecorationPatch
  | XDecorationCentroid
  | XDecorationSample
  | XDecorationInvariant
  | XDecorationRestrict
  | XDecorationAliased
  | XDecorationVolatile
  | XDecorationConstant
  | XDecorationCoherent
  | XDecorationNonWritable
  | XDecorationNonReadable
  | XDecorationUniform
  | XDecorationSaturatedConversion
  | XDecorationStream                       WORD
  | XDecorationLocation                     WORD
  | XDecorationComponent                    WORD
  | XDecorationIndex                        WORD
  | XDecorationBinding                      WORD
  | XDecorationDescriptorSet                WORD
  | XDecorationOffset                       WORD
  | XDecorationXfbBuffer                    WORD
  | XDecorationXfbStride                    WORD
  | XDecorationFuncParamAttr                FunctionParameterAttribute
  | XDecorationFPRoundingMode               FPRoundingMode
  | XDecorationFPFastMathMode               FPFastMathMode
  | XDecorationLinkageAttributes            STRING LinkageType
  | XDecorationNoContraction
  | XDecorationInputAttachmentIndex         WORD
  #if 0
  | XDecorationAlignment                    WORD
  | XDecorationMaxByteOffset                WORD
  | XDecorationAlignmentId                  Id
  | XDecorationMaxByteOffsetId              Id
  #else
  | XDecorationAlignment                    NumLit
  | XDecorationMaxByteOffset                NumLit
  #endif
  | XDecorationExplicitInterpAMD
  | XDecorationOverrideCoverageNV
  | XDecorationPassthroughNV
  | XDecorationViewportRelativeNV
  | XDecorationSecondaryViewportRelativeNV  WORD
  __D
-- }}}
instance To Decoration XDecoration where
  -- {{{
  to f (DecorationAlignmentId x) = XDecorationAlignment (f x)
  to f (DecorationMaxByteOffsetId x) = XDecorationMaxByteOffset (f x)
  to _ (DecorationAlignment x) = XDecorationAlignment (toNumLit x)
  to _ (DecorationMaxByteOffset x) = XDecorationMaxByteOffset (toNumLit x)
  to _ (DecorationSpecId val) = XDecorationSpecId val
  to _ (DecorationLinkageAttributes name linktype) = XDecorationLinkageAttributes name linktype
  to _ (DecorationBuiltIn builtin) = XDecorationBuiltIn builtin
  to _ (DecorationFuncParamAttr fpattr) = XDecorationFuncParamAttr fpattr
  to _ (DecorationFPRoundingMode x) = XDecorationFPRoundingMode x
  to _ (DecorationFPFastMathMode x) = XDecorationFPFastMathMod x
  to _ (DecorationArrayStride word) = XDecorationArrayStride word
  to _ (DecorationMatrixStride word) = XDecorationMatrixStride word
  to _ (DecorationStream word) = XDecorationStream word
  to _ (DecorationLocation word) = XDecorationLocation word
  to _ (DecorationComponent word) = XDecorationComponent word
  to _ (DecorationIndex word) = XDecorationIndex word
  to _ (DecorationBinding word) = XDecorationBinding word
  to _ (DecorationDescriptorSet word) = XDecorationDescriptorSet word
  to _ (DecorationOffset word) = XDecorationOffset word
  to _ (DecorationXfbBuffer word) = XDecorationXfbBuffer word
  to _ (DecorationXfbStride word) = XDecorationXfbStride word
  to _ (DecorationInputAttachmentIndex word) = XDecorationInputAttachmentIndex word
  to _ (DecorationSecondaryViewportRelativeNV word) = XDecorationSecondaryViewportRelativeNV word
  to _ (DecorationRelaxedPrecision) = XDecorationRelaxedPrecision
  to _ (DecorationBlock) = XDecorationBlock
  to _ (DecorationBufferBlock) = XDecorationBufferBlock
  to _ (DecorationRowMajor) = XDecorationRowMajor
  to _ (DecorationColMajor) = XDecorationColMajor
  to _ (DecorationGLSLShared) = XDecorationGLSLShared
  to _ (DecorationGLSLPacked) = XDecorationGLSLPacked
  to _ (DecorationCPacked) = XDecorationCPacked
  to _ (DecorationNoPerspective) = XDecorationNoPerspective
  to _ (DecorationFlat) = XDecorationFlat
  to _ (DecorationPatch) = XDecorationPatch
  to _ (DecorationCentroid) = XDecorationCentroid
  to _ (DecorationSample) = XDecorationSample
  to _ (DecorationInvariant) = XDecorationInvariant
  to _ (DecorationRestrict) = XDecorationRestrict
  to _ (DecorationAliased) = XDecorationAliased
  to _ (DecorationVolatile) = XDecorationVolatile
  to _ (DecorationConstant) = XDecorationConstant
  to _ (DecorationCoherent) = XDecorationCoherent
  to _ (DecorationNonWritable) = XDecorationNonWritable
  to _ (DecorationNonReadable) = XDecorationNonReadable
  to _ (DecorationUniform) = XDecorationUniform
  to _ (DecorationSaturatedConversion) = XDecorationSaturatedConversion
  to _ (DecorationNoContraction) = XDecorationNoContraction
  to _ (DecorationExplicitInterpAMD) = XDecorationExplicitInterpAMD
  to _ (DecorationOverrideCoverageNV) = XDecorationOverrideCoverageNV
  to _ (DecorationPassthroughNV) = XDecorationPassthroughNV
  to _ (DecorationViewportRelativeNV) = XDecorationViewportRelativeNV
  -- }}}
instance (AddNumLit env) => From env Decoration XDecoration where
  -- {{{
  from env (XDecorationAlignment x) = case x of
    Num1L w-> (DecorationAlignment w, env)
    _ | (new,env) <- addNumLit env x
      -> (XDecorationAlignmentId new, env)
  from env (XDecorationMaxByteOffset x) = case x of
    Num1L w-> (DecorationMaxByteOffset w, env)
    _ | (new,env) <- addNumLit env x
      -> (XDecorationMaxByteOffsetId new, env)
  from env (XDecorationSpecId val) = (DecorationSpecId val, env)
  from env (XDecorationLinkageAttributes name linktype) = (DecorationLinkageAttributes name linktype, env)
  from env (XDecorationBuiltIn builtin) = (DecorationBuiltIn builtin, env)
  from env (XDecorationFuncParamAttr fpattr) = (DecorationFuncParamAttr fpattr, env)
  from env (XDecorationFPRoundingMode x) = (DecorationFPRoundingMode x, env)
  from env (XDecorationFPFastMathMode x) = (DecorationFPFastMathMod x, env)
  from env (XDecorationArrayStride word) = (DecorationArrayStride word, env)
  from env (XDecorationMatrixStride word) = (DecorationMatrixStride word, env)
  from env (XDecorationStream word) = (DecorationStream word, env)
  from env (XDecorationLocation word) = (DecorationLocation word, env)
  from env (XDecorationComponent word) = (DecorationComponent word, env)
  from env (XDecorationIndex word) = (DecorationIndex word, env)
  from env (XDecorationBinding word) = (DecorationBinding word, env)
  from env (XDecorationDescriptorSet word) = (DecorationDescriptorSet word, env)
  from env (XDecorationOffset word) = (DecorationOffset word, env)
  from env (XDecorationXfbBuffer word) = (DecorationXfbBuffer word, env)
  from env (XDecorationXfbStride word) = (DecorationXfbStride word, env)
  from env (XDecorationInputAttachmentIndex word) = (DecorationInputAttachmentIndex word, env)
  from env (XDecorationSecondaryViewportRelativeNV word) = (DecorationSecondaryViewportRelativeNV word, env)
  from env (XDecorationRelaxedPrecision) = (DecorationRelaxedPrecision, env)
  from env (XDecorationBlock) = (DecorationBlock, env)
  from env (XDecorationBufferBlock) = (DecorationBufferBlock, env)
  from env (XDecorationRowMajor) = (DecorationRowMajor, env)
  from env (XDecorationColMajor) = (DecorationColMajor, env)
  from env (XDecorationGLSLShared) = (DecorationGLSLShared, env)
  from env (XDecorationGLSLPacked) = (DecorationGLSLPacked, env)
  from env (XDecorationCPacked) = (DecorationCPacked, env)
  from env (XDecorationNoPerspective) = (DecorationNoPerspective, env)
  from env (XDecorationFlat) = (DecorationFlat, env)
  from env (XDecorationPatch) = (DecorationPatch, env)
  from env (XDecorationCentroid) = (DecorationCentroid, env)
  from env (XDecorationSample) = (DecorationSample, env)
  from env (XDecorationInvariant) = (DecorationInvariant, env)
  from env (XDecorationRestrict) = (DecorationRestrict, env)
  from env (XDecorationAliased) = (DecorationAliased, env)
  from env (XDecorationVolatile) = (DecorationVolatile, env)
  from env (XDecorationConstant) = (DecorationConstant, env)
  from env (XDecorationCoherent) = (DecorationCoherent, env)
  from env (XDecorationNonWritable) = (DecorationNonWritable, env)
  from env (XDecorationNonReadable) = (DecorationNonReadable, env)
  from env (XDecorationUniform) = (DecorationUniform, env)
  from env (XDecorationSaturatedConversion) = (DecorationSaturatedConversion, env)
  from env (XDecorationNoContraction) = (DecorationNoContraction, env)
  from env (XDecorationExplicitInterpAMD) = (DecorationExplicitInterpAMD, env)
  from env (XDecorationOverrideCoverageNV) = (DecorationOverrideCoverageNV, env)
  from env (XDecorationPassthroughNV) = (DecorationPassthroughNV, env)
  from env (XDecorationViewportRelativeNV) = (DecorationViewportRelativeNV, env)
  -- }}}

data XExecutionMode
-- {{{
  = XExecutionModeInvocations LiteralInteger
  | XExecutionModeSpacingEqual
  | XExecutionModeSpacingFractionalEven
  | XExecutionModeSpacingFractionalOdd
  | XExecutionModeVertexOrderCw
  | XExecutionModeVertexOrderCcw
  | XExecutionModePixelCenterInteger
  | XExecutionModeOriginUpperLeft
  | XExecutionModeOriginLowerLeft
  | XExecutionModeEarlyFragmentTests
  | XExecutionModePointMode
  | XExecutionModeXfb
  | XExecutionModeDepthReplacing
  | XExecutionModeDepthGreater
  | XExecutionModeDepthLess
  | XExecutionModeDepthUnchanged
  | XExecutionModeLocalSize LiteralInteger LiteralInteger LiteralInteger
  | XExecutionModeLocalSizeHint LiteralInteger LiteralInteger LiteralInteger
  | XExecutionModeInputPoints
  | XExecutionModeInputLines
  | XExecutionModeInputLinesAdjacency
  | XExecutionModeTriangles
  | XExecutionModeInputTrianglesAdjacency
  | XExecutionModeQuads
  | XExecutionModeIsolines
  | XExecutionModeOutputVertices LiteralInteger
  | XExecutionModeOutputPoints
  | XExecutionModeOutputLineStrip
  | XExecutionModeOutputTriangleStrip
  | XExecutionModeVecTypeHint LiteralInteger
  | XExecutionModeContractionOff
  | XExecutionModeInitializer
  | XExecutionModeFinalizer
  | XExecutionModeSubgroupSize LiteralInteger
  | XExecutionModeSubgroupsPerWorkgroup LiteralInteger
  | XExecutionModeSubgroupsPerWorkgroupId Id
  | XExecutionModeLocalSizeId Id Id Id
  | XExecutionModeLocalSizeHintId Id
  | XExecutionModePostDepthCoverage
  __D
-- }}}
instance To ExecutionMode XExecutionMode where
-- {{{
  to f (ExecutionModeSubgroupsPerWorkgroupId x) = XExecutionModeSubgroupsPerWorkgroup (f x)
  to f (ExecutionModeLocalSizeId x1 x2 x3) = XExecutionModeLocalSize (f x1) (f x2) (f x3)
  to f (ExecutionModeLocalSizeHintId x) = __FIXME("ExecutionModeLocalSizeHintId")
  to _ (ExecutionModeSubgroupsPerWorkgroup x) = XExecutionModeSubgroupsPerWorkgroup (toNumLit x)
  to _ (ExecutionModeLocalSize x1 x2 x3) = XExecutionModeLocalSize (toNumLit x1) (toNumLit x2) (toNumLit x3)
  to _ (ExecutionModeLocalSizeHint x1 x2 x3) = XExecutionModeLocalSizeHint (toNumLit x1) (toNumLit x2) (toNumLit x3)
  to _ (ExecutionModeInvocations x) = XExecutionModeInvocations x
  to _ (ExecutionModeOutputVertices x) = XExecutionModeOutputVertices x
  to _ (ExecutionModeVecTypeHint x) = XExecutionModeVecTypeHint x
  to _ (ExecutionModeSubgroupSize x) = XExecutionModeSubgroupSize x
  to _ (XExecutionModeSpacingEqual) = XXExecutionModeSpacingEqual
  to _ (XExecutionModeSpacingFractionalEven) = XXExecutionModeSpacingFractionalEven
  to _ (XExecutionModeSpacingFractionalOdd) = XXExecutionModeSpacingFractionalOdd
  to _ (XExecutionModeVertexOrderCw) = XXExecutionModeVertexOrderCw
  to _ (XExecutionModeVertexOrderCcw) = XXExecutionModeVertexOrderCcw
  to _ (XExecutionModePixelCenterInteger) = XXExecutionModePixelCenterInteger
  to _ (XExecutionModeOriginUpperLeft) = XXExecutionModeOriginUpperLeft
  to _ (XExecutionModeOriginLowerLeft) = XXExecutionModeOriginLowerLeft
  to _ (XExecutionModeEarlyFragmentTests) = XXExecutionModeEarlyFragmentTests
  to _ (XExecutionModePointMode) = XXExecutionModePointMode
  to _ (XExecutionModeXfb) = XXExecutionModeXfb
  to _ (XExecutionModeDepthReplacing) = XXExecutionModeDepthReplacing
  to _ (XExecutionModeDepthGreater) = XXExecutionModeDepthGreater
  to _ (XExecutionModeDepthLess) = XXExecutionModeDepthLess
  to _ (XExecutionModeDepthUnchanged) = XXExecutionModeDepthUnchanged
  to _ (XExecutionModeInputPoints) = XXExecutionModeInputPoints
  to _ (XExecutionModeInputLines) = XXExecutionModeInputLines
  to _ (XExecutionModeInputLinesAdjacency) = XXExecutionModeInputLinesAdjacency
  to _ (XExecutionModeTriangles) = XXExecutionModeTriangles
  to _ (XExecutionModeInputTrianglesAdjacency) = XXExecutionModeInputTrianglesAdjacency
  to _ (XExecutionModeQuads) = XXExecutionModeQuads
  to _ (XExecutionModeIsolines) = XXExecutionModeIsolines
  to _ (XExecutionModeOutputPoints) = XXExecutionModeOutputPoints
  to _ (XExecutionModeOutputLineStrip) = XXExecutionModeOutputLineStrip
  to _ (XExecutionModeOutputTriangleStrip) = XXExecutionModeOutputTriangleStrip
  to _ (XExecutionModeContractionOff) = XXExecutionModeContractionOff
  to _ (XExecutionModeInitializer) = XXExecutionModeInitializer
  to _ (XExecutionModeFinalizer) = XXExecutionModeFinalizer
  to _ (XExecutionModePostDepthCoverage) = XXExecutionModePostDepthCoverage
-- }}}
instance (AddNumLit env) => From env ExecutionMode XExecutionMode where
-- {{{
  from env (XExecutionModeSubgroupsPerWorkgroup x1) = case x1 of
    Num1L w1
      -> (ExecutionModeSubgroupsPerWorkgroup w1, env)
    _ | (y1,env) <- addNumLit env x1
      -> (ExecutionModeSubgroupsPerWorkgroupId y1, env)
  from env (XExecutionModeLocalSize x1 x2 x3) = case (x1,x2,x3) of
    (Num1L w1,Num1L w2,Num1L w3)
      -> (ExecutionModeLocalSize w1 w2 w3, env)
    _ | (y1,env) <- addNumLit env x1
      , (y2,env) <- addNumLit env x2
      , (y3,env) <- addNumLit env x3
      -> (ExecutionModeLocalSizeId y1 y2 y3, env)
  from env (XExecutionModeLocalSizeHint x1 x2 x3) = case (x1,x2,x3) of
    (Num1L w1,Num1L w2,Num1L w3)
      -> (ExecutionModeLocalSizeHint w1 w2 w3, env)
    _ -> __FIXME("from<XExecutionModeLocalSizeHint{Id}>")
  from env (XExecutionModeInvocations x) = (ExecutionModeInvocations x, env)
  from env (XExecutionModeOutputVertices x) = (ExecutionModeOutputVertices x, env)
  from env (XExecutionModeVecTypeHint x) = (ExecutionModeVecTypeHint x, env)
  from env (XExecutionModeSubgroupSize x) = (ExecutionModeSubgroupSize x, env)
  from env (XExecutionModeSpacingEqual) = (ExecutionModeSpacingEqual, env)
  from env (XExecutionModeSpacingFractionalEven) = (ExecutionModeSpacingFractionalEven, env)
  from env (XExecutionModeSpacingFractionalOdd) = (ExecutionModeSpacingFractionalOdd, env)
  from env (XExecutionModeVertexOrderCw) = (ExecutionModeVertexOrderCw, env)
  from env (XExecutionModeVertexOrderCcw) = (ExecutionModeVertexOrderCcw, env)
  from env (XExecutionModePixelCenterInteger) = (ExecutionModePixelCenterInteger, env)
  from env (XExecutionModeOriginUpperLeft) = (ExecutionModeOriginUpperLeft, env)
  from env (XExecutionModeOriginLowerLeft) = (ExecutionModeOriginLowerLeft, env)
  from env (XExecutionModeEarlyFragmentTests) = (ExecutionModeEarlyFragmentTests, env)
  from env (XExecutionModePointMode) = (ExecutionModePointMode, env)
  from env (XExecutionModeXfb) = (ExecutionModeXfb, env)
  from env (XExecutionModeDepthReplacing) = (ExecutionModeDepthReplacing, env)
  from env (XExecutionModeDepthGreater) = (ExecutionModeDepthGreater, env)
  from env (XExecutionModeDepthLess) = (ExecutionModeDepthLess, env)
  from env (XExecutionModeDepthUnchanged) = (ExecutionModeDepthUnchanged, env)
  from env (XExecutionModeInputPoints) = (ExecutionModeInputPoints, env)
  from env (XExecutionModeInputLines) = (ExecutionModeInputLines, env)
  from env (XExecutionModeInputLinesAdjacency) = (ExecutionModeInputLinesAdjacency, env)
  from env (XExecutionModeTriangles) = (ExecutionModeTriangles, env)
  from env (XExecutionModeInputTrianglesAdjacency) = (ExecutionModeInputTrianglesAdjacency, env)
  from env (XExecutionModeQuads) = (ExecutionModeQuads, env)
  from env (XExecutionModeIsolines) = (ExecutionModeIsolines, env)
  from env (XExecutionModeOutputPoints) = (ExecutionModeOutputPoints, env)
  from env (XExecutionModeOutputLineStrip) = (ExecutionModeOutputLineStrip, env)
  from env (XExecutionModeOutputTriangleStrip) = (ExecutionModeOutputTriangleStrip, env)
  from env (XExecutionModeContractionOff) = (ExecutionModeContractionOff, env)
  from env (XExecutionModeInitializer) = (ExecutionModeInitializer, env)
  from env (XExecutionModeFinalizer) = (ExecutionModeFinalizer, env)
  from env (XExecutionModePostDepthCoverage) = (ExecutionModePostDepthCoverage, env)
-- }}}

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

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
type OpKey = [WORD]

-----------------------------------------------------------------------------

data Op
  = OpNop
  | OpUndef IdResultType IdResult
  | OpSourceContinued LiteralString
  | OpSource SourceLanguage LiteralInteger (Maybe IdRef) (Maybe LiteralString)
  | OpSourceExtension LiteralString
  | OpName IdRef LiteralString
  | OpMemberName IdRef LiteralInteger LiteralString
  | OpString IdResult LiteralString
  | OpLine IdRef LiteralInteger LiteralInteger
  | OpExtension LiteralString
  | OpExtInstImport IdResult LiteralString
  | OpExtInst IdResultType IdResult IdRef LiteralExtInstInteger [IdRef]
  | OpMemoryModel AddressingModel MemoryModel
  | OpEntryPoint ExecutionModel IdRef LiteralString [IdRef]
  | OpExecutionMode IdRef ExecutionMode
  | OpCapability Capability
  | OpTypeVoid IdResult
  | OpTypeBool IdResult
  | OpTypeInt IdResult LiteralInteger LiteralInteger
  | OpTypeFloat IdResult LiteralInteger
  | OpTypeVector IdResult IdRef LiteralInteger
  | OpTypeMatrix IdResult IdRef LiteralInteger
  | OpTypeImage IdResult IdRef Dim LiteralInteger LiteralInteger LiteralInteger LiteralInteger ImageFormat (Maybe AccessQualifier)
  | OpTypeSampler IdResult
  | OpTypeSampledImage IdResult IdRef
  | OpTypeArray IdResult IdRef IdRef
  | OpTypeRuntimeArray IdResult IdRef
  | OpTypeStruct IdResult [IdRef]
  | OpTypeOpaque IdResult LiteralString
  | OpTypePointer IdResult StorageClass IdRef
  | OpTypeFunction IdResult IdRef [IdRef]
  | OpTypeEvent IdResult
  | OpTypeDeviceEvent IdResult
  | OpTypeReserveId IdResult
  | OpTypeQueue IdResult
  | OpTypePipe IdResult AccessQualifier
  | OpTypeForwardPointer IdRef StorageClass
  | OpConstantTrue IdResultType IdResult
  | OpConstantFalse IdResultType IdResult
  | OpConstant IdResultType IdResult LiteralContextDependentNumber
  | OpConstantComposite IdResultType IdResult [IdRef]
  | OpConstantSampler IdResultType IdResult SamplerAddressingMode LiteralInteger SamplerFilterMode
  | OpConstantNull IdResultType IdResult
  | OpSpecConstantTrue IdResultType IdResult
  | OpSpecConstantFalse IdResultType IdResult
  | OpSpecConstant IdResultType IdResult LiteralContextDependentNumber
  | OpSpecConstantComposite IdResultType IdResult [IdRef]
  | OpSpecConstantOp IdResultType IdResult LiteralSpecConstantOpInteger [IdRef]
  | OpFunction IdResultType IdResult FunctionControl IdRef
  | OpFunctionParameter IdResultType IdResult
  | OpFunctionEnd
  | OpFunctionCall IdResultType IdResult IdRef [IdRef]
  | OpVariable IdResultType IdResult StorageClass (Maybe IdRef)
  | OpImageTexelPointer IdResultType IdResult IdRef IdRef IdRef
  | OpLoad IdResultType IdResult IdRef (Maybe MemoryAccess)
  | OpStore IdRef IdRef (Maybe MemoryAccess)
  | OpCopyMemory IdRef IdRef (Maybe MemoryAccess)
  | OpCopyMemorySized IdRef IdRef IdRef (Maybe MemoryAccess)
  | OpAccessChain IdResultType IdResult IdRef [IdRef]
  | OpInBoundsAccessChain IdResultType IdResult IdRef [IdRef]
  | OpPtrAccessChain IdResultType IdResult IdRef IdRef [IdRef]
  | OpArrayLength IdResultType IdResult IdRef LiteralInteger
  | OpGenericPtrMemSemantics IdResultType IdResult IdRef
  | OpInBoundsPtrAccessChain IdResultType IdResult IdRef IdRef [IdRef]
  | OpDecorate IdRef Decoration
  | OpMemberDecorate IdRef LiteralInteger Decoration
  | OpDecorationGroup IdResult
  | OpGroupDecorate IdRef [IdRef]
  | OpGroupMemberDecorate IdRef [PairIdRefLiteralInteger]
  | OpVectorExtractDynamic IdResultType IdResult IdRef IdRef
  | OpVectorInsertDynamic IdResultType IdResult IdRef IdRef IdRef
  | OpVectorShuffle IdResultType IdResult IdRef IdRef [LiteralInteger]
  | OpCompositeConstruct IdResultType IdResult [IdRef]
  | OpCompositeExtract IdResultType IdResult IdRef [LiteralInteger]
  | OpCompositeInsert IdResultType IdResult IdRef IdRef [LiteralInteger]
  | OpCopyObject IdResultType IdResult IdRef
  | OpTranspose IdResultType IdResult IdRef
  | OpSampledImage IdResultType IdResult IdRef IdRef
  | OpImageSampleImplicitLod IdResultType IdResult IdRef IdRef (Maybe ImageOperands)
  | OpImageSampleExplicitLod IdResultType IdResult IdRef IdRef ImageOperands
  | OpImageSampleDrefImplicitLod IdResultType IdResult IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImageSampleDrefExplicitLod IdResultType IdResult IdRef IdRef IdRef ImageOperands
  | OpImageSampleProjImplicitLod IdResultType IdResult IdRef IdRef (Maybe ImageOperands)
  | OpImageSampleProjExplicitLod IdResultType IdResult IdRef IdRef ImageOperands
  | OpImageSampleProjDrefImplicitLod IdResultType IdResult IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImageSampleProjDrefExplicitLod IdResultType IdResult IdRef IdRef IdRef ImageOperands
  | OpImageFetch IdResultType IdResult IdRef IdRef (Maybe ImageOperands)
  | OpImageGather IdResultType IdResult IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImageDrefGather IdResultType IdResult IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImageRead IdResultType IdResult IdRef IdRef (Maybe ImageOperands)
  | OpImageWrite IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImage IdResultType IdResult IdRef
  | OpImageQueryFormat IdResultType IdResult IdRef
  | OpImageQueryOrder IdResultType IdResult IdRef
  | OpImageQuerySizeLod IdResultType IdResult IdRef IdRef
  | OpImageQuerySize IdResultType IdResult IdRef
  | OpImageQueryLod IdResultType IdResult IdRef IdRef
  | OpImageQueryLevels IdResultType IdResult IdRef
  | OpImageQuerySamples IdResultType IdResult IdRef
  | OpConvertFToU IdResultType IdResult IdRef
  | OpConvertFToS IdResultType IdResult IdRef
  | OpConvertSToF IdResultType IdResult IdRef
  | OpConvertUToF IdResultType IdResult IdRef
  | OpUConvert IdResultType IdResult IdRef
  | OpSConvert IdResultType IdResult IdRef
  | OpFConvert IdResultType IdResult IdRef
  | OpQuantizeToF16 IdResultType IdResult IdRef
  | OpConvertPtrToU IdResultType IdResult IdRef
  | OpSatConvertSToU IdResultType IdResult IdRef
  | OpSatConvertUToS IdResultType IdResult IdRef
  | OpConvertUToPtr IdResultType IdResult IdRef
  | OpPtrCastToGeneric IdResultType IdResult IdRef
  | OpGenericCastToPtr IdResultType IdResult IdRef
  | OpGenericCastToPtrExplicit IdResultType IdResult IdRef StorageClass
  | OpBitcast IdResultType IdResult IdRef
  | OpSNegate IdResultType IdResult IdRef
  | OpFNegate IdResultType IdResult IdRef
  | OpIAdd IdResultType IdResult IdRef IdRef
  | OpFAdd IdResultType IdResult IdRef IdRef
  | OpISub IdResultType IdResult IdRef IdRef
  | OpFSub IdResultType IdResult IdRef IdRef
  | OpIMul IdResultType IdResult IdRef IdRef
  | OpFMul IdResultType IdResult IdRef IdRef
  | OpUDiv IdResultType IdResult IdRef IdRef
  | OpSDiv IdResultType IdResult IdRef IdRef
  | OpFDiv IdResultType IdResult IdRef IdRef
  | OpUMod IdResultType IdResult IdRef IdRef
  | OpSRem IdResultType IdResult IdRef IdRef
  | OpSMod IdResultType IdResult IdRef IdRef
  | OpFRem IdResultType IdResult IdRef IdRef
  | OpFMod IdResultType IdResult IdRef IdRef
  | OpVectorTimesScalar IdResultType IdResult IdRef IdRef
  | OpMatrixTimesScalar IdResultType IdResult IdRef IdRef
  | OpVectorTimesMatrix IdResultType IdResult IdRef IdRef
  | OpMatrixTimesVector IdResultType IdResult IdRef IdRef
  | OpMatrixTimesMatrix IdResultType IdResult IdRef IdRef
  | OpOuterProduct IdResultType IdResult IdRef IdRef
  | OpDot IdResultType IdResult IdRef IdRef
  | OpIAddCarry IdResultType IdResult IdRef IdRef
  | OpISubBorrow IdResultType IdResult IdRef IdRef
  | OpUMulExtended IdResultType IdResult IdRef IdRef
  | OpSMulExtended IdResultType IdResult IdRef IdRef
  | OpAny IdResultType IdResult IdRef
  | OpAll IdResultType IdResult IdRef
  | OpIsNan IdResultType IdResult IdRef
  | OpIsInf IdResultType IdResult IdRef
  | OpIsFinite IdResultType IdResult IdRef
  | OpIsNormal IdResultType IdResult IdRef
  | OpSignBitSet IdResultType IdResult IdRef
  | OpLessOrGreater IdResultType IdResult IdRef IdRef
  | OpOrdered IdResultType IdResult IdRef IdRef
  | OpUnordered IdResultType IdResult IdRef IdRef
  | OpLogicalEqual IdResultType IdResult IdRef IdRef
  | OpLogicalNotEqual IdResultType IdResult IdRef IdRef
  | OpLogicalOr IdResultType IdResult IdRef IdRef
  | OpLogicalAnd IdResultType IdResult IdRef IdRef
  | OpLogicalNot IdResultType IdResult IdRef
  | OpSelect IdResultType IdResult IdRef IdRef IdRef
  | OpIEqual IdResultType IdResult IdRef IdRef
  | OpINotEqual IdResultType IdResult IdRef IdRef
  | OpUGreaterThan IdResultType IdResult IdRef IdRef
  | OpSGreaterThan IdResultType IdResult IdRef IdRef
  | OpUGreaterThanEqual IdResultType IdResult IdRef IdRef
  | OpSGreaterThanEqual IdResultType IdResult IdRef IdRef
  | OpULessThan IdResultType IdResult IdRef IdRef
  | OpSLessThan IdResultType IdResult IdRef IdRef
  | OpULessThanEqual IdResultType IdResult IdRef IdRef
  | OpSLessThanEqual IdResultType IdResult IdRef IdRef
  | OpFOrdEqual IdResultType IdResult IdRef IdRef
  | OpFUnordEqual IdResultType IdResult IdRef IdRef
  | OpFOrdNotEqual IdResultType IdResult IdRef IdRef
  | OpFUnordNotEqual IdResultType IdResult IdRef IdRef
  | OpFOrdLessThan IdResultType IdResult IdRef IdRef
  | OpFUnordLessThan IdResultType IdResult IdRef IdRef
  | OpFOrdGreaterThan IdResultType IdResult IdRef IdRef
  | OpFUnordGreaterThan IdResultType IdResult IdRef IdRef
  | OpFOrdLessThanEqual IdResultType IdResult IdRef IdRef
  | OpFUnordLessThanEqual IdResultType IdResult IdRef IdRef
  | OpFOrdGreaterThanEqual IdResultType IdResult IdRef IdRef
  | OpFUnordGreaterThanEqual IdResultType IdResult IdRef IdRef
  | OpShiftRightLogical IdResultType IdResult IdRef IdRef
  | OpShiftRightArithmetic IdResultType IdResult IdRef IdRef
  | OpShiftLeftLogical IdResultType IdResult IdRef IdRef
  | OpBitwiseOr IdResultType IdResult IdRef IdRef
  | OpBitwiseXor IdResultType IdResult IdRef IdRef
  | OpBitwiseAnd IdResultType IdResult IdRef IdRef
  | OpNot IdResultType IdResult IdRef
  | OpBitFieldInsert IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpBitFieldSExtract IdResultType IdResult IdRef IdRef IdRef
  | OpBitFieldUExtract IdResultType IdResult IdRef IdRef IdRef
  | OpBitReverse IdResultType IdResult IdRef
  | OpBitCount IdResultType IdResult IdRef
  | OpDPdx IdResultType IdResult IdRef
  | OpDPdy IdResultType IdResult IdRef
  | OpFwidth IdResultType IdResult IdRef
  | OpDPdxFine IdResultType IdResult IdRef
  | OpDPdyFine IdResultType IdResult IdRef
  | OpFwidthFine IdResultType IdResult IdRef
  | OpDPdxCoarse IdResultType IdResult IdRef
  | OpDPdyCoarse IdResultType IdResult IdRef
  | OpFwidthCoarse IdResultType IdResult IdRef
  | OpEmitVertex
  | OpEndPrimitive
  | OpEmitStreamVertex IdRef
  | OpEndStreamPrimitive IdRef
  | OpControlBarrier IdScope IdScope IdMemorySemantics
  | OpMemoryBarrier IdScope IdMemorySemantics
  | OpAtomicLoad IdResultType IdResult IdRef IdScope IdMemorySemantics
  | OpAtomicStore IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicExchange IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicCompareExchange IdResultType IdResult IdRef IdScope IdMemorySemantics IdMemorySemantics IdRef IdRef
  | OpAtomicCompareExchangeWeak IdResultType IdResult IdRef IdScope IdMemorySemantics IdMemorySemantics IdRef IdRef
  | OpAtomicIIncrement IdResultType IdResult IdRef IdScope IdMemorySemantics
  | OpAtomicIDecrement IdResultType IdResult IdRef IdScope IdMemorySemantics
  | OpAtomicIAdd IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicISub IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicSMin IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicUMin IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicSMax IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicUMax IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicAnd IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicOr IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpAtomicXor IdResultType IdResult IdRef IdScope IdMemorySemantics IdRef
  | OpPhi IdResultType IdResult [PairIdRefIdRef]
  | OpLoopMerge IdRef IdRef LoopControl
  | OpSelectionMerge IdRef SelectionControl
  | OpLabel IdResult
  | OpBranch IdRef
  | OpBranchConditional IdRef IdRef IdRef [LiteralInteger]
  | OpSwitch IdRef IdRef [(LiteralContextDependentNumber, IdRef)]
  | OpKill
  | OpReturn
  | OpReturnValue IdRef
  | OpUnreachable
  | OpLifetimeStart IdRef LiteralInteger
  | OpLifetimeStop IdRef LiteralInteger
  | OpGroupAsyncCopy IdResultType IdResult IdScope IdRef IdRef IdRef IdRef IdRef
  | OpGroupWaitEvents IdScope IdRef IdRef
  | OpGroupAll IdResultType IdResult IdScope IdRef
  | OpGroupAny IdResultType IdResult IdScope IdRef
  | OpGroupBroadcast IdResultType IdResult IdScope IdRef IdRef
  | OpGroupIAdd IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupFAdd IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupFMin IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupUMin IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupSMin IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupFMax IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupUMax IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupSMax IdResultType IdResult IdScope GroupOperation IdRef
  | OpReadPipe IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpWritePipe IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpReservedReadPipe IdResultType IdResult IdRef IdRef IdRef IdRef IdRef IdRef
  | OpReservedWritePipe IdResultType IdResult IdRef IdRef IdRef IdRef IdRef IdRef
  | OpReserveReadPipePackets IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpReserveWritePipePackets IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpCommitReadPipe IdRef IdRef IdRef IdRef
  | OpCommitWritePipe IdRef IdRef IdRef IdRef
  | OpIsValidReserveId IdResultType IdResult IdRef
  | OpGetNumPipePackets IdResultType IdResult IdRef IdRef IdRef
  | OpGetMaxPipePackets IdResultType IdResult IdRef IdRef IdRef
  | OpGroupReserveReadPipePackets IdResultType IdResult IdScope IdRef IdRef IdRef IdRef
  | OpGroupReserveWritePipePackets IdResultType IdResult IdScope IdRef IdRef IdRef IdRef
  | OpGroupCommitReadPipe IdScope IdRef IdRef IdRef IdRef
  | OpGroupCommitWritePipe IdScope IdRef IdRef IdRef IdRef
  | OpEnqueueMarker IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpEnqueueKernel IdResultType IdResult IdRef IdRef IdRef IdRef IdRef IdRef IdRef IdRef IdRef IdRef [IdRef]
  | OpGetKernelNDrangeSubGroupCount IdResultType IdResult IdRef IdRef IdRef IdRef IdRef
  | OpGetKernelNDrangeMaxSubGroupSize IdResultType IdResult IdRef IdRef IdRef IdRef IdRef
  | OpGetKernelWorkGroupSize IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpGetKernelPreferredWorkGroupSizeMultiple IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpRetainEvent IdRef
  | OpReleaseEvent IdRef
  | OpCreateUserEvent IdResultType IdResult
  | OpIsValidEvent IdResultType IdResult IdRef
  | OpSetUserEventStatus IdRef IdRef
  | OpCaptureEventProfilingInfo IdRef IdRef IdRef
  | OpGetDefaultQueue IdResultType IdResult
  | OpBuildNDRange IdResultType IdResult IdRef IdRef IdRef
  | OpImageSparseSampleImplicitLod IdResultType IdResult IdRef IdRef (Maybe ImageOperands)
  | OpImageSparseSampleExplicitLod IdResultType IdResult IdRef IdRef ImageOperands
  | OpImageSparseSampleDrefImplicitLod IdResultType IdResult IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImageSparseSampleDrefExplicitLod IdResultType IdResult IdRef IdRef IdRef ImageOperands
  | OpImageSparseSampleProjImplicitLod IdResultType IdResult IdRef IdRef (Maybe ImageOperands)
  | OpImageSparseSampleProjExplicitLod IdResultType IdResult IdRef IdRef ImageOperands
  | OpImageSparseSampleProjDrefImplicitLod IdResultType IdResult IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImageSparseSampleProjDrefExplicitLod IdResultType IdResult IdRef IdRef IdRef ImageOperands
  | OpImageSparseFetch IdResultType IdResult IdRef IdRef (Maybe ImageOperands)
  | OpImageSparseGather IdResultType IdResult IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImageSparseDrefGather IdResultType IdResult IdRef IdRef IdRef (Maybe ImageOperands)
  | OpImageSparseTexelsResident IdResultType IdResult IdRef
  | OpNoLine
  | OpAtomicFlagTestAndSet IdResultType IdResult IdRef IdScope IdMemorySemantics
  | OpAtomicFlagClear IdRef IdScope IdMemorySemantics
  | OpImageSparseRead IdResultType IdResult IdRef IdRef (Maybe ImageOperands)
  | OpSizeOf IdResultType IdResult IdRef
  | OpTypePipeStorage IdResult
  | OpConstantPipeStorage IdResultType IdResult LiteralInteger LiteralInteger LiteralInteger
  | OpCreatePipeFromPipeStorage IdResultType IdResult IdRef
  | OpGetKernelLocalSizeForSubgroupCount IdResultType IdResult IdRef IdRef IdRef IdRef IdRef
  | OpGetKernelMaxNumSubgroups IdResultType IdResult IdRef IdRef IdRef IdRef
  | OpTypeNamedBarrier IdResult
  | OpNamedBarrierInitialize IdResultType IdResult IdRef
  | OpMemoryNamedBarrier IdRef IdScope IdMemorySemantics
  | OpModuleProcessed LiteralString
  | OpExecutionModeId IdRef ExecutionMode
  | OpDecorateId IdRef Decoration
  | OpSubgroupBallotKHR IdResultType IdResult IdRef
  | OpSubgroupFirstInvocationKHR IdResultType IdResult IdRef
  | OpSubgroupAllKHR IdResultType IdResult IdRef
  | OpSubgroupAnyKHR IdResultType IdResult IdRef
  | OpSubgroupAllEqualKHR IdResultType IdResult IdRef
  | OpSubgroupReadInvocationKHR IdResultType IdResult IdRef IdRef
  | OpGroupIAddNonUniformAMD IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupFAddNonUniformAMD IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupFMinNonUniformAMD IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupUMinNonUniformAMD IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupSMinNonUniformAMD IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupFMaxNonUniformAMD IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupUMaxNonUniformAMD IdResultType IdResult IdScope GroupOperation IdRef
  | OpGroupSMaxNonUniformAMD IdResultType IdResult IdScope GroupOperation IdRef

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
--
-- Debug Related
--

data SourceLanguage
  = SourceLanguageUnknown
  | SourceLanguageESSL
  | SourceLanguageGLSL
  | SourceLanguageOpenCL_C
  | SourceLanguageOpenCL_CPP
  | SourceLanguageHLSL

-----------------------------------------------------------------------------
--
-- Global ATTRS/MD
--

data Capability
-- {{{
  = CapabilityMatrix
  | CapabilityShader
  | CapabilityGeometry
  | CapabilityTessellation
  | CapabilityAddresses
  | CapabilityLinkage
  | CapabilityKernel
  | CapabilityVector16
  | CapabilityFloat16Buffer
  | CapabilityFloat16
  | CapabilityFloat64
  | CapabilityInt64
  | CapabilityInt64Atomics
  | CapabilityImageBasic
  | CapabilityImageReadWrite
  | CapabilityImageMipmap
  | CapabilityPipes
  | CapabilityGroups
  | CapabilityDeviceEnqueue
  | CapabilityLiteralSampler
  | CapabilityAtomicStorage
  | CapabilityInt16
  | CapabilityTessellationPointSize
  | CapabilityGeometryPointSize
  | CapabilityImageGatherExtended
  | CapabilityStorageImageMultisample
  | CapabilityUniformBufferArrayDynamicIndexing
  | CapabilitySampledImageArrayDynamicIndexing
  | CapabilityStorageBufferArrayDynamicIndexing
  | CapabilityStorageImageArrayDynamicIndexing
  | CapabilityClipDistance
  | CapabilityCullDistance
  | CapabilityImageCubeArray
  | CapabilitySampleRateShading
  | CapabilityImageRect
  | CapabilitySampledRect
  | CapabilityGenericPointer
  | CapabilityInt8
  | CapabilityInputAttachment
  | CapabilitySparseResidency
  | CapabilityMinLod
  | CapabilitySampled1D
  | CapabilityImage1D
  | CapabilitySampledCubeArray
  | CapabilitySampledBuffer
  | CapabilityImageBuffer
  | CapabilityImageMSArray
  | CapabilityStorageImageExtendedFormats
  | CapabilityImageQuery
  | CapabilityDerivativeControl
  | CapabilityInterpolationFunction
  | CapabilityTransformFeedback
  | CapabilityGeometryStreams
  | CapabilityStorageImageReadWithoutFormat
  | CapabilityStorageImageWriteWithoutFormat
  | CapabilityMultiViewport
  | CapabilitySubgroupDispatch
  | CapabilityNamedBarrier
  | CapabilityPipeStorage
  | CapabilitySubgroupBallotKHR
  | CapabilityDrawParameters
  | CapabilitySubgroupVoteKHR
  | CapabilityStorageBuffer16BitAccess
  | CapabilityStorageUniformBufferBlock16
  | CapabilityUniformAndStorageBuffer16BitAccess
  | CapabilityStorageUniform16
  | CapabilityStoragePushConstant16
  | CapabilityStorageInputOutput16
  | CapabilityDeviceGroup
  | CapabilityMultiView
  | CapabilityVariablePointersStorageBuffer
  | CapabilityVariablePointers
  | CapabilityAtomicStorageOps
  | CapabilitySampleMaskPostDepthCoverage
  | CapabilityImageGatherBiasLodAMD
  | CapabilityStencilExportEXT
  | CapabilitySampleMaskOverrideCoverageNV
  | CapabilityGeometryShaderPassthroughNV
  | CapabilityShaderViewportIndexLayerNV
  | CapabilityShaderViewportIndexLayerEXT
  | CapabilityShaderViewportMaskNV
  | CapabilityShaderStereoViewNV
  | CapabilityPerViewAttributesNV
-- }}}

data MemoryModel
  = MemoryModelSimple
  | MemoryModelGLSL450
  | MemoryModelOpenCL

data AddressingModel
  = AddressingModelLogical
  | AddressingModelPhysical32
  | AddressingModelPhysical64

-----------------------------------------------------------------------------
--
-- Linkage, ATTRS, MD, __?__
--

--
-- <<LINKAGE>>
--     LinkageAttributes
-- <<BUILTINS>>
--     BuiltIn
-- SUBATTRS<F>
--     FuncParamAttr
--     ATTRS<O>
-- ATTRS<I>
--     FPRoundingMode
--     FPFastMathMode
--     SaturatedConversion
-- ATTRS<T>
--     CPacked
--     GLSLShared
--     GLSLPacked
--     Alignment
--     MaxByteOffset
--     AlignmentId
--     MaxByteOffsetId
-- ATTRS<O>
--     Alignment
--     MaxByteOffset
--     AlignmentId
--     MaxByteOffsetId
--     Restrict
--     Aliased
--     Volatile
--     Constant
--     Coherent
--     NonWritable
--     NonReadable
-- DEFN<C>
--     SpecId
-- <<__TODO__>>
--     RelaxedPrecision
--     Block
--     BufferBlock
--     RowMajor
--     ColMajor
--     ArrayStride
--     MatrixStride
--     NoPerspective
--     Flat
--     Patch
--     Centroid
--     Sample
--     Invariant
--     Uniform
--     Stream
--     Location
--     Component
--     Index
--     Binding
--     DescriptorSet
--     Offset
--     XfbBuffer
--     XfbStride
--     NoContraction
--     InputAttachmentIndex
--     ExplicitInterpAMD
--     OverrideCoverageNV
--     PassthroughNV
--     ViewportRelativeNV
--     SecondaryViewportRelativeNV
--
data Decoration
-- {{{
  = DecorationRelaxedPrecision
  | DecorationSpecId LiteralInteger
  | DecorationBlock
  | DecorationBufferBlock
  | DecorationRowMajor
  | DecorationColMajor
  | DecorationArrayStride LiteralInteger
  | DecorationMatrixStride LiteralInteger
  | DecorationGLSLShared
  | DecorationGLSLPacked
  | DecorationCPacked
  | DecorationBuiltIn BuiltIn
  | DecorationNoPerspective
  | DecorationFlat
  | DecorationPatch
  | DecorationCentroid
  | DecorationSample
  | DecorationInvariant
  | DecorationRestrict
  | DecorationAliased
  | DecorationVolatile
  | DecorationConstant
  | DecorationCoherent
  | DecorationNonWritable
  | DecorationNonReadable
  | DecorationUniform
  | DecorationSaturatedConversion
  | DecorationStream LiteralInteger
  | DecorationLocation LiteralInteger
  | DecorationComponent LiteralInteger
  | DecorationIndex LiteralInteger
  | DecorationBinding LiteralInteger
  | DecorationDescriptorSet LiteralInteger
  | DecorationOffset LiteralInteger
  | DecorationXfbBuffer LiteralInteger
  | DecorationXfbStride LiteralInteger
  | DecorationFuncParamAttr FunctionParameterAttribute
  | DecorationFPRoundingMode FPRoundingMode
  | DecorationFPFastMathMode FPFastMathMode
  | DecorationLinkageAttributes LiteralString LinkageType
  | DecorationNoContraction
  | DecorationInputAttachmentIndex LiteralInteger
  | DecorationAlignment LiteralInteger
  | DecorationMaxByteOffset LiteralInteger
  | DecorationAlignmentId Id
  | DecorationMaxByteOffsetId Id
  | DecorationExplicitInterpAMD
  | DecorationOverrideCoverageNV
  | DecorationPassthroughNV
  | DecorationViewportRelativeNV
  | DecorationSecondaryViewportRelativeNV LiteralInteger
-- }}}

-----------------------------------------------------------------------------
--
-- Linkage, EntryPoints, etc.
--

data LinkageType
  = LinkageTypeExport
  | LinkageTypeImport

data ExecutionModel
  = ExecutionModelVertex
  | ExecutionModelTessellationControl
  | ExecutionModelTessellationEvaluation
  | ExecutionModelGeometry
  | ExecutionModelFragment
  | ExecutionModelGLCompute
  | ExecutionModelKernel

data ExecutionMode
-- {{{
  = ExecutionModeInvocations LiteralInteger
  | ExecutionModeSpacingEqual
  | ExecutionModeSpacingFractionalEven
  | ExecutionModeSpacingFractionalOdd
  | ExecutionModeVertexOrderCw
  | ExecutionModeVertexOrderCcw
  | ExecutionModePixelCenterInteger
  | ExecutionModeOriginUpperLeft
  | ExecutionModeOriginLowerLeft
  | ExecutionModeEarlyFragmentTests
  | ExecutionModePointMode
  | ExecutionModeXfb
  | ExecutionModeDepthReplacing
  | ExecutionModeDepthGreater
  | ExecutionModeDepthLess
  | ExecutionModeDepthUnchanged
  | ExecutionModeLocalSize LiteralInteger LiteralInteger LiteralInteger
  | ExecutionModeLocalSizeHint LiteralInteger LiteralInteger LiteralInteger
  | ExecutionModeInputPoints
  | ExecutionModeInputLines
  | ExecutionModeInputLinesAdjacency
  | ExecutionModeTriangles
  | ExecutionModeInputTrianglesAdjacency
  | ExecutionModeQuads
  | ExecutionModeIsolines
  | ExecutionModeOutputVertices LiteralInteger
  | ExecutionModeOutputPoints
  | ExecutionModeOutputLineStrip
  | ExecutionModeOutputTriangleStrip
  | ExecutionModeVecTypeHint LiteralInteger
  | ExecutionModeContractionOff
  | ExecutionModeInitializer
  | ExecutionModeFinalizer
  | ExecutionModeSubgroupSize LiteralInteger
  | ExecutionModeSubgroupsPerWorkgroup LiteralInteger
  | ExecutionModeSubgroupsPerWorkgroupId Id
  | ExecutionModeLocalSizeId Id Id Id
  | ExecutionModeLocalSizeHintId Id
  | ExecutionModePostDepthCoverage
-- }}}

data BuiltIn
-- {{{
  = BuiltInPosition
  | BuiltInPointSize
  | BuiltInClipDistance
  | BuiltInCullDistance
  | BuiltInVertexId
  | BuiltInInstanceId
  | BuiltInPrimitiveId
  | BuiltInInvocationId
  | BuiltInLayer
  | BuiltInViewportIndex
  | BuiltInTessLevelOuter
  | BuiltInTessLevelInner
  | BuiltInTessCoord
  | BuiltInPatchVertices
  | BuiltInFragCoord
  | BuiltInPointCoord
  | BuiltInFrontFacing
  | BuiltInSampleId
  | BuiltInSamplePosition
  | BuiltInSampleMask
  | BuiltInFragDepth
  | BuiltInHelperInvocation
  | BuiltInNumWorkgroups
  | BuiltInWorkgroupSize
  | BuiltInWorkgroupId
  | BuiltInLocalInvocationId
  | BuiltInGlobalInvocationId
  | BuiltInLocalInvocationIndex
  | BuiltInWorkDim
  | BuiltInGlobalSize
  | BuiltInEnqueuedWorkgroupSize
  | BuiltInGlobalOffset
  | BuiltInGlobalLinearId
  | BuiltInSubgroupSize
  | BuiltInSubgroupMaxSize
  | BuiltInNumSubgroups
  | BuiltInNumEnqueuedSubgroups
  | BuiltInSubgroupId
  | BuiltInSubgroupLocalInvocationId
  | BuiltInVertexIndex
  | BuiltInInstanceIndex
  | BuiltInSubgroupEqMaskKHR
  | BuiltInSubgroupGeMaskKHR
  | BuiltInSubgroupGtMaskKHR
  | BuiltInSubgroupLeMaskKHR
  | BuiltInSubgroupLtMaskKHR
  | BuiltInBaseVertex
  | BuiltInBaseInstance
  | BuiltInDrawIndex
  | BuiltInDeviceIndex
  | BuiltInViewIndex
  | BuiltInBaryCoordNoPerspAMD
  | BuiltInBaryCoordNoPerspCentroidAMD
  | BuiltInBaryCoordNoPerspSampleAMD
  | BuiltInBaryCoordSmoothAMD
  | BuiltInBaryCoordSmoothCentroidAMD
  | BuiltInBaryCoordSmoothSampleAMD
  | BuiltInBaryCoordPullModelAMD
  | BuiltInFragStencilRefEXT
  | BuiltInViewportMaskNV
  | BuiltInSecondaryPositionNV
  | BuiltInSecondaryViewportMaskNV
  | BuiltInPositionPerViewNV
  | BuiltInViewportMaskPerViewNV
-- }}}

-----------------------------------------------------------------------------
--
-- ATTRS, MD, ADDRSPACE
--

--
-- ADDRSPACE <- ALL
--
data StorageClass
  = StorageClassUniformConstant
  | StorageClassInput
  | StorageClassUniform
  | StorageClassOutput
  | StorageClassWorkgroup
  | StorageClassCrossWorkgroup
  | StorageClassPrivate
  | StorageClassFunction
  | StorageClassGeneric
  | StorageClassPushConstant
  | StorageClassAtomicCounter
  | StorageClassImage
  | StorageClassStorageBuffer

--
-- ATTRS<F> <- [Pure,Const]
-- MD<F> <- [Inline,DontInline]
--
data FunctionControl = FunctionControl
  {functionControlInline :: Bool
  ,functionControlDontInline :: Bool
  ,functionControlPure :: Bool
  ,functionControlConst :: Bool}

--
-- SUBATTRS<F> <- ALL
--
data FunctionParameterAttribute
  = FunctionParameterAttributeZext
  | FunctionParameterAttributeSext
  | FunctionParameterAttributeByVal
  | FunctionParameterAttributeSret
  | FunctionParameterAttributeNoAlias
  | FunctionParameterAttributeNoCapture
  | FunctionParameterAttributeNoWrite
  | FunctionParameterAttributeNoReadWrite

--
-- ATTRS<T> <- ALL
--
data AccessQualifier
  = AccessQualifierReadOnly
  | AccessQualifierWriteOnly
  | AccessQualifierReadWrite

--
-- MD<I> <- ALL
--
data SelectionControl = SelectionControl
  {selectionControlFlatten :: Bool
  ,selectionControlDontFlatten :: Bool}
data LoopControl = LoopControl
  {loopControlUnroll :: Bool
  ,loopControlDontUnroll :: Bool
  ,loopControlDependencyInfinite :: Bool
  ,loopControlDependencyLength :: Maybe LiteralInteger}

--
-- ATTRS<I> <- ALL
--
data MemoryAccess = MemoryAccess
  {memoryAccessVolatile :: Bool
  ,memoryAccessAligned :: Maybe LiteralInteger
  ,memoryAccessNontemporal :: Bool}
data FPRoundingMode
  = FPRoundingModeRTE
  | FPRoundingModeRTZ
  | FPRoundingModeRTP
  | FPRoundingModeRTN
data FPFastMathMode = FPFastMathMode
  {fPFastMathModeNotNaN :: Bool
  ,fPFastMathModeNotInf :: Bool
  ,fPFastMathModeNSZ :: Bool
  ,fPFastMathModeAllowRecip :: Bool
  ,fPFastMathModeFast :: Bool}

--
-- ATTRS<I> <- DEREF_CST_ID(ALL)
--
data MemorySemantics = MemorySemantics
  {memorySemanticsAcquire :: Bool
  ,memorySemanticsRelease :: Bool
  ,memorySemanticsAcquireRelease :: Bool
  ,memorySemanticsSequentiallyConsistent :: Bool
  ,memorySemanticsUniformMemory :: Bool
  ,memorySemanticsSubgroupMemory :: Bool
  ,memorySemanticsWorkgroupMemory :: Bool
  ,memorySemanticsCrossWorkgroupMemory :: Bool
  ,memorySemanticsAtomicCounterMemory :: Bool
  ,memorySemanticsImageMemory :: Bool}
data Scope
  = ScopeCrossDevice
  | ScopeDevice
  | ScopeWorkgroup
  | ScopeSubgroup
  | ScopeInvocation

-----------------------------------------------------------------------------
--
-- Help determine OP and/or OPERANDS
--

--
-- OP <- f(..,ALL,..)
--
data GroupOperation
  = GroupOperationReduce
  | GroupOperationInclusiveScan
  | GroupOperationExclusiveScan

--
-- OP_cst <- f(OpConstantSamplerTAG,ALL,..)
--
data SamplerAddressingMode
  = SamplerAddressingModeNone
  | SamplerAddressingModeClampToEdge
  | SamplerAddressingModeClamp
  | SamplerAddressingModeRepeat
  | SamplerAddressingModeRepeatMirrored
data SamplerFilterMode
  = SamplerFilterModeNearest
  | SamplerFilterModeLinear

--
-- OPERANDS += ALL
--
data ImageOperands = ImageOperands
  {imageOperandsBias :: Maybe Id
  ,imageOperandsLod :: Maybe Id
  ,imageOperandsGrad :: Maybe (Id, Id)
  ,imageOperandsConstOffset :: Maybe Id
  ,imageOperandsOffset :: Maybe Id
  ,imageOperandsConstOffsets :: Maybe Id
  ,imageOperandsSample :: Maybe Id
  ,imageOperandsMinLod :: Maybe Id}

-----------------------------------------------------------------------------
--
-- ENUM ARGS
--

--
-- ENUM INT args of OpTypeImage[2] and OpTypeImage[7]
--
data Dim = Dim1D | Dim2D | Dim3D | DimCube | DimRect | DimBuffer | DimSubpassData
data ImageFormat = ImageFormatUnknown | ImageFormatRgba32f | ImageFormatRgba16f | ImageFormatR32f | ImageFormatRgba8 | ImageFormatRgba8Snorm | ImageFormatRg32f | ImageFormatRg16f | ImageFormatR11fG11fB10f | ImageFormatR16f | ImageFormatRgba16 | ImageFormatRgb10A2 | ImageFormatRg16 | ImageFormatRg8 | ImageFormatR16 | ImageFormatR8 | ImageFormatRgba16Snorm | ImageFormatRg16Snorm | ImageFormatRg8Snorm | ImageFormatR16Snorm | ImageFormatR8Snorm | ImageFormatRgba32i | ImageFormatRgba16i | ImageFormatRgba8i | ImageFormatR32i | ImageFormatRg32i | ImageFormatRg16i | ImageFormatRg8i | ImageFormatR16i | ImageFormatR8i | ImageFormatRgba32ui | ImageFormatRgba16ui | ImageFormatRgba8ui | ImageFormatR32ui | ImageFormatRgb10a2ui | ImageFormatRg32ui | ImageFormatRg16ui | ImageFormatRg8ui | ImageFormatR16ui | ImageFormatR8ui

--
-- The ENUM INT arg for OpEnqueueKernel[3]
--
data KernelEnqueueFlags
  = KernelEnqueueFlagsNoWait
  | KernelEnqueueFlagsWaitKernel
  | KernelEnqueueFlagsWaitWorkGroup

--
-- The ENUM INT arg for OpCaptureEventProfilingInfo[1]
--
data KernelProfilingInfo = KernelProfilingInfo
  {kernelProfilingInfoCmdExecTime :: Bool}

-----------------------------------------------------------------------------
--
-- ENUM RESULTS
--

--
-- The ENUM INT result of OpImageQueryOrder
--
data ImageChannelOrder
  = ImageChannelOrderR
  | ImageChannelOrderA
  | ImageChannelOrderRG
  | ImageChannelOrderRA
  | ImageChannelOrderRGB
  | ImageChannelOrderRGBA
  | ImageChannelOrderBGRA
  | ImageChannelOrderARGB
  | ImageChannelOrderIntensity
  | ImageChannelOrderLuminance
  | ImageChannelOrderRx
  | ImageChannelOrderRGx
  | ImageChannelOrderRGBx
  | ImageChannelOrderDepth
  | ImageChannelOrderDepthStencil
  | ImageChannelOrdersRGB
  | ImageChannelOrdersRGBx
  | ImageChannelOrdersRGBA
  | ImageChannelOrdersBGRA
  | ImageChannelOrderABGR

--
-- The ENUM INT result of OpImageQueryFormat
--
data ImageChannelDataType
  = ImageChannelDataTypeSnormInt8
  | ImageChannelDataTypeSnormInt16
  | ImageChannelDataTypeUnormInt8
  | ImageChannelDataTypeUnormInt16
  | ImageChannelDataTypeUnormShort565
  | ImageChannelDataTypeUnormShort555
  | ImageChannelDataTypeUnormInt101010
  | ImageChannelDataTypeSignedInt8
  | ImageChannelDataTypeSignedInt16
  | ImageChannelDataTypeSignedInt32
  | ImageChannelDataTypeUnsignedInt8
  | ImageChannelDataTypeUnsignedInt16
  | ImageChannelDataTypeUnsignedInt32
  | ImageChannelDataTypeHalfFloat
  | ImageChannelDataTypeFloat
  | ImageChannelDataTypeUnormInt24
  | ImageChannelDataTypeUnormInt101010_2

-----------------------------------------------------------------------------
```

## Two

```haskell
-----------------------------------------------------------------------------

#define CONFIG_SIZEOF_WORD 4
#define CONFIG_MAX_NUMLIT_WORDS 4
decorationsKey
  :: (Id -> [WORD])
  -> [Decoration]
  -> [[Decoration]]
  -> [WORD]
decorationsKey words ds dss = hashToWords . Bin.encode . fmap one $ ds:dss
  where one :: [Decoration] -> B.ByteString
        one = l2s . Bin.encode . uniqSort . fmap (decorationKey words)
        uniqSort = OS.toList . OS.fromList
        l2s = B.concat . L.toChunks
decorationKey :: (Id -> [WORD]) -> Decoration -> [WORD]
decorationKey words d = case d of
  DecorationLinkageAttributes{}-> []
  DecorationAlignmentId x-> DecorationAlignmentIdTAG:words x
  DecorationMaxByteOffsetId x-> DecorationMaxByteOffsetIdTAG:words x
  _ | (x,xs) <- toValEnum d-> x : (toWords =<< xs)
hashToWords :: L.ByteString -> [WORD]
hashToWords = Bin.runGet (replicateM 4 Bin.getWord32le) . Bin.encode . md5 . l2s
  where l2s = B.concat . L.toChunks
padToMaxNumLitWords :: NumLit -> [WORD]
padToMaxNumLitWords x = case x of
  Num1L w-> replicate (CONFIG_MAX_NUMLIT_WORDS - 1) 0 ++ [w]
  NumNL ws-> replicate (CONFIG_MAX_NUMLIT_WORDS - length ws) 0 ++ ws

#if 0
ghci> let attrs = ([],[[DecorationFuncParamAttr FunctionParameterAttributeNoAlias],[DecorationFuncParamAttr FunctionParameterAttributeZext],[]])::([Decoration],[[Decoration]])
ghci> :t attrs
attrs :: ([Decoration], [[Decoration]])
ghci> :t uncurry (decorationsKey undefined) attrs
uncurry (decorationsKey undefined) attrs :: [WORD]
ghci> ppHs attrs
([],
 [[DecorationFuncParamAttr FunctionParameterAttributeNoAlias],
  [DecorationFuncParamAttr FunctionParameterAttributeZext],
  []])
ghci> ppHs $ uncurry (decorationsKey undefined) attrs
[1581721998, 830482446, 4278086272, 2918980240]
#endif

-----------------------------------------------------------------------------

signature SPIRV_LAYOUT = sig
  -- | Given the basicblock graph for a function, along with the entry block,
  -- return the nodes sorted in a way that respects dominance.
  layoutBB :: Graph -> Node -> Result [Node]
  -- | Toposorts the SCCs of the {Type,Constant} graph, asserts any cyclic
  -- SCC contains ONLY Types and NO Constants, asserts that all cycles pass
  -- through an @OpTypePointer@ node, and inserts @OpForwardPointer@s as
  -- appropriate for cyclic SCCs.
  layoutTC
    :: Graph    -- Types and Constants
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
  getGraphBB :: Context -> IdMap (Graph, Node)
  getGraphTC :: Context -> (Graph, NodeSet, NodeSet)
  getGraphGV :: Context -> Graph
  getGraphDG :: Context -> Graph

-----------------------------------------------------------------------------

signature DRIVER = sig
  type Config
  type Options
  usage :: IO ExitCode
  main :: Config -> Options -> IO ExitCode

-----------------------------------------------------------------------------

signature VALENV = sig
  data VAL
  type AbsOp
  type AbsInsnKey
  instance Eq AbsOp
  instance Ord AbsOp
  instance Bounded AbsOp
  instance Enum AbsOp
  instance Eq AbsInsnKey
  instance Ord AbsInsnKey
  -- | `BIPARTITE([PORT,BOT,TOP],[INSN,VALS])`
  data Value
    = PORT !(Port VAL)
    | INSN AbsOp AbsInsnKey [Ix VAL]
    | VALS [Ix VAL]
    | BOT
    | TOP
  instance Eq Value
  instance Ord Value
  instance Bot Value
  instance Top Value
  class
    (NewM m (Ix VAL)
    ,GetM (Ix ID) (Ix VAL)
    ,GetM (Ix INSN) (Ix VAL)
    ,AddM (Ix ID) (Ix VAL)
    ,AddM (Ix INSN) (Ix VAL)
    ,UFM m (Ix VAL) Value
    ,TopM m (Ix VAL)
    ,MeetM m (Ix VAL)
    ) => ValEnvBaseM m
  class (ValEnvBaseM m) => ValEnvM m where
    getUsesM :: Ix VAL -> m (IxMap VAL PosSet)
    getPartRefineM :: m (PartRefine VAL)

-----------------------------------------------------------------------------

#if 0
ConT AccessQualifier
ConT AddressingModel
ConT Capability
ConT Dim
ConT ExecutionModel
ConT FunctionControl
ConT GroupOperation
ConT ImageFormat
ConT LiteralContextDependentNumber
ConT LiteralExtInstInteger
ConT LiteralInteger
ConT LiteralSpecConstantOpInteger
ConT LiteralString
ConT LoopControl
ConT MemoryModel
ConT SamplerAddressingMode
ConT SamplerFilterMode
ConT SelectionControl
ConT SourceLanguage
ConT StorageClass
AppT (ConT Maybe) (ConT AccessQualifier)
AppT (ConT Maybe) (ConT LiteralString)
AppT (ConT Maybe) (ConT MemoryAccess)
AppT ListT (ConT LiteralInteger)
#endif

#if 0
ConT IdResult
ConT IdResultType
ConT IdRef
ConT IdScope
ConT IdMemorySemantics
ConT ImageOperands
ConT Decoration
ConT ExecutionMode
AppT (ConT Maybe) (ConT IdRef)
AppT (ConT Maybe) (ConT ImageOperands)
AppT ListT (ConT IdRef)
AppT ListT (ConT PairIdRefIdRef)
AppT ListT (ConT PairIdRefLiteralInteger)
AppT ListT (ConT PairLiteralContextDependentNumberIdRef)
#endif

signature SPIRV_TO_FROM = sig
  type Attrs
  type MD

  type Result = Either [STRING]
  class ToFromSPIRV env a b where
    toSPIRV :: env -> a -> Result b
    fromSPIRV :: env -> b -> Result a

  instance ToFromSPIRV () Annot SPIRVAnnot where
    toSPIRV () Annot{..}
      = Left []
    fromSPIRV () SPIRVAnnot{..}
      = Left []
  data Annot = Annot
    {annotAttrs :: Attrs
    ,annotMD :: MD
    }
  data SPIRVAnnot = SPIRVAnnot
    {spirvAnnotFunCtl :: Maybe FunctionControl
    ,spirvAnnotExecMode :: Set XExecutionMode
    ,spirvAnnotD :: Set XDecoration
    ,spirvAnnotSubD :: PosMap (Set XDecoration)
    ,spirvAnnotN :: Set Name
    ,spirvAnnotSubN :: PosMap (Set Name)
    }
  data XDecoration
  data XExecuctionMode

-----------------------------------------------------------------------------

signature DB_SPIRV = sig
  data INSN
  data ID
  data DB = DB
    {
    -- WHO THE FUCK CARES
    }

-----------------------------------------------------------------------------

signature MM_MISC = sig
  data Asm = Asm
    {asmVolatile  :: Bool
    ,asmCode      :: StringCst
    ,asmOutputs   :: [AsmOutput]
    ,asmInputs    :: [AsmInput]
    ,asmClobbers  :: [AsmClobber]
    ,asmTargets   :: [Id]} __D
  type AsmOutput  = (StringCst,Exp)
  type AsmInput   = (StringCst,Exp)
  type AsmClobber = StringCst

signature MM_MD = sig
  type MD

signature MM_ATTRS = sig
  --type TQ
  --instance O TQ
  --instance Lat TQ
  --allTQ :: TQ
  --emptyTQ :: TQ
  --constTQ :: TQ
  --restrictTQ :: TQ
  --volatileTQ :: TQ
  --type FS
  --instance O FS
  --instance Lat FS
  --allFS :: FS
  --emptyFS :: FS
  --inlineFS :: FS
  --noreturnFS :: FS
  --pureFS :: FS
  --constFS :: FS
  --mallocFS :: FS
  --type SS
  --autoSS :: SS
  --registerSS :: SS
  --externSS :: SS
  --staticSS :: SS
  --externThreadSS :: SS
  --staticThreadSS :: SS

-----------------------------------------------------------------------------

signature MM_CST = sig
  instance IsIntCst [WORD]
  instance IsFloatCst [WORD]
  class IsIntCst a where
    toIntCst :: BitSize -> a -> IntCst
    fromIntCst :: IntCst -> (a, BitSize)
  class IsFloatCst a where
    toFloatCst :: BitSize -> a -> FloatCst
    fromFloatCst :: FloatCst -> (a, BitSize)
  data UndefCst = UndefCst __D
  data BoolCst = BoolCst !Int __D
  data IntCst = IntCst !BitSize !Int __D
  data FloatCst = FloatCst !BitSize !MPFR deriving(Eq,Ord,Show)
  data VectorCst = VectorCst !Size [Ix CST] __D
  data ArrayCst = ArrayCst !Size [Ix CST] __D
  data StructCst = StructCst [Ix CST] __D
  data StringCst = StringCst !STRING __D
  instance Read FloatCst where
    readsPrec = __FIXME("instance Read FloatCst")
  #define BIN16_PREC  (10+1)
  #define BIN32_PREC  (23+1)
  #define BIN64_PREC  (52+1)
  #define BIN128_PREC (112+1)

signature MM_TYPE = sig
  type Type
  type TypeKind

signature DB_MM_0 = sig
  ---------------------
  type ATTRS
  type MD
  type Cst
  type Type
  type TypeKind
  type CFG
  type DefnB
  type DefnI
  type DefnX
  type SpecB
  type SpecI
  type SpecX
  ---------------------
-- {{{
  data MOD
  data TYPE
  data CST
  data OBJ
  data FUN
  data BLOCK
  data INSN
  data ID
  type M = MOD
  type T = TYPE
  type C = CST
  type O = OBJ
  type F = FUN
  type B = BLOCK
  type I = INSN
  type X = ID
  ---------------------
  data Defn a = DeclD | DefnD a __D
  data Attrs = Attrs {aAttrs :: ATTRS, aSubAttrs :: PosMap ATTRS}
  ---------------------
  type DefnT = Type
  type DefnC = Cst
  type DefnO = Defn (Maybe ObjInit)
  type DefnF = Defn CFG
  data ObjInit = NoneOI | ObjOI !(Ix OBJ) | CstOI !(Ix CST)
  ---------------------
  data SpecT = SpecT
    {stAttrs :: Attrs
    ,stKind :: TypeKind}
  type SpecC = SpecX
  data SpecO = SpecO
    {soAttrs :: Attrs
    ,soType :: !(Ix T)
    ,soStorage :: StorageClass}
  data SpecF = SpecF
    {sfAttrs :: Attrs
    ,sfType :: !(Ix T)
    ,sfCtl :: FunctionControl}
  ---------------------
  data Mod = Mod
    {modNAll :: N ()
    ,modNRoots :: N ()
    ,modIface :: ModIface
    ,modBlockCst :: !(Ix BLOCK)
    }
  data ModHdr = ModHdr
    {modHdrCaps :: Set Capability
    ,modHdrExts :: Set Name
    }
  data ModIface = ModIface
    {miExports :: Map Name LinkEnt
    ,miImports :: Map Name LinkEnt
    ,miEntryPoints :: NameMap (Map ExecutionModel (Ix FUN))
    ,miExecutionModes :: IxMap FUN (Set ExecutionMode)}
  data LinkEnt = FunLE !(Ix FUN) | ObjLE !(Ix OBJ)
  --data SymBind = STB_LOCAL | STB_GLOBAL | STB_WEAK | STB_EXTERN __D_BE
  --data SymType = STT_NOTYPE | STT_OBJECT | STT_FUNC | STT_COMMON | STT_TLS __D_BE
  ---------------------
  data Ent = EntM !(Ix MOD) | EntT !(Ix T) | EntC !(Ix C) | EntO !(Ix O) | EntF !(Ix F) | EntB !(Ix B) | EntI !(Ix I) | EntX !(Ix X)
  data Rn = Rn {rnT :: IxMap T (Ix T), rnC :: IxMap C (Ix C), rnO :: IxMap O (Ix O), rnF :: IxMap F (Ix F), rnB :: IxMap B (Ix B), rnI :: IxMap I (Ix I), rnX :: IxMap X (Ix X)}
  data Rns = Rn {rnsT :: IxMap T (IxMap T ()), rnsC :: IxMap C (IxMap C ()), rnsO :: IxMap O (IxMap O ()), rnsF :: IxMap F (IxMap F ()), rnsB :: IxMap B (IxMap B ()), rnsI :: IxMap I (IxMap I ()), rnsX :: IxMap X (IxMap X ())}
  data N a = N {nT :: IxMap T a, nC :: IxMap C a, nO :: IxMap O a, nF :: IxMap F a, nB :: IxMap B a, nI :: IxMap I a, nX :: IxMap X a}
  data NQ a = NQ {nqT :: UF T a, nqC :: UF C a, nqO :: UF O a, nqF :: UF F a, nqB :: UF B a, nqI :: UF I a, nqX :: UF X a}
  -- {{{
  instance GetSetLook Rn (Ix T) (Ix T)
  instance GetSetLook Rn (Ix C) (Ix C)
  instance GetSetLook Rn (Ix O) (Ix O)
  instance GetSetLook Rn (Ix F) (Ix F)
  instance GetSetLook Rn (Ix B) (Ix B)
  instance GetSetLook Rn (Ix I) (Ix I)
  instance GetSetLook Rn (Ix X) (Ix X)
  instance GetSetLook Rns (Ix T) (IxMap T ())
  instance GetSetLook Rns (Ix C) (IxMap C ())
  instance GetSetLook Rns (Ix O) (IxMap O ())
  instance GetSetLook Rns (Ix F) (IxMap F ())
  instance GetSetLook Rns (Ix B) (IxMap B ())
  instance GetSetLook Rns (Ix I) (IxMap I ())
  instance GetSetLook Rns (Ix X) (IxMap X ())
  instance GetSetLook (N a) (Ix T) a
  instance GetSetLook (N a) (Ix C) a
  instance GetSetLook (N a) (Ix O) a
  instance GetSetLook (N a) (Ix F) a
  instance GetSetLook (N a) (Ix B) a
  instance GetSetLook (N a) (Ix I) a
  instance GetSetLook (N a) (Ix X) a
  instance Monoid Rn
  instance Monoid Rns
  instance Monoid (N a)
  instance Empty Rn
  instance Empty Rns
  instance Empty (N a)
  instance Empty (NQ a)
  instance Join Rn
  instance Join Rns
  instance (Join a) => Join (N a)
  instance Meet Rn
  instance Meet Rns
  instance (Meet a) => Meet (N a)
  instance Diff Rn
  instance Diff Rns
  instance (Diff a) => Diff (N a)
  instance O Rn
  instance O Rns
  instance (O a) => O (N a)
  instance M.Codom Rn (N a)
  instance M.Codom Rns (N a)
  instance M.Compose Rn Rn Rn
  instance M.Compose Rns Rns Rns
  instance M.Dom Rn (N a)
  instance M.Dom Rns (N a)
  instance M.Img Rn (N a) (N a)
  instance M.Img Rns (N a) (N a)
  instance M.InvImg Rn Rns
  instance M.InvImg Rns Rns
  instance M.Restrict Rn (N a)
  instance M.Restrict Rns (N a)
  #define UF(x) UF.x
  INSTANCE_UF(UF,(NQ a),T,a,NQ,nqT)
  INSTANCE_UF(UF,(NQ a),C,a,NQ,nqC)
  INSTANCE_UF(UF,(NQ a),O,a,NQ,nqO)
  INSTANCE_UF(UF,(NQ a),F,a,NQ,nqF)
  INSTANCE_UF(UF,(NQ a),B,a,NQ,nqB)
  INSTANCE_UF(UF,(NQ a),I,a,NQ,nqI)
  INSTANCE_UF(UF,(NQ a),X,a,NQ,nqX)
  #undef UF
  -- }}}
  ---------------------
  data DB = DB
    {dbNext   :: !Int
    ,dbMod    :: ModDB
    ,dbSpec   :: SpecDB
    ,dbDefn   :: DefnDB
    ,dbMD     :: N MD
    ,dbParent :: N Ent} __D
  data DBHdr = DBHdr
    {dbHdrMemModel :: (MemoryModel, AddressingModel)
    ,dbHdrCaps :: Set Capability
    ,dbHdrExts :: Set Name
    }
  data ModDB = ModDB
    {dbModM :: IxMap M Mod} __D
  data SpecDB = SpecDB {dbSpecT :: IxMap T SpecT, dbSpecC :: IxMap C SpecC, dbSpecO :: IxMap O SpecO, dbSpecF :: IxMap F SpecF, dbSpecB :: IxMap B SpecB, dbSpecI :: IxMap I SpecI, dbSpecX :: IxMap X SpecX} __D
  data DefnDB = DefnDB {dbDefnT :: IxMap T DefnT, dbDefnC :: IxMap C DefnC, dbDefnO :: IxMap O DefnO, dbDefnF :: IxMap F DefnF, dbDefnB :: IxMap B DefnB, dbDefnI :: IxMap I DefnI, dbDefnX :: IxMap X DefnX} __D
  -- {{{
  instance Monoid DB
  instance Monoid ModDB
  instance Monoid SpecDB
  instance Monoid DefnDB
  instance Empty DB
  instance Empty ModDB
  instance Empty SpecDB
  instance Empty DefnDB
  instance Join DB
  instance Join ModDB
  instance Join SpecDB
  instance Join DefnDB
  instance Meet DB
  instance Meet ModDB
  instance Meet SpecDB
  instance Meet DefnDB
  instance Diff DB
  instance Diff ModDB
  instance Diff SpecDB
  instance Diff DefnDB
  instance O DB
  instance O ModDB
  instance O SpecDB
  instance O DefnDB
  instance GetSetLook DB (Ix M) Mod
  instance GetSetLook DB (Ix T) SpecT
  instance GetSetLook DB (Ix C) SpecC
  instance GetSetLook DB (Ix O) SpecO
  instance GetSetLook DB (Ix F) SpecF
  instance GetSetLook DB (Ix B) SpecB
  instance GetSetLook DB (Ix I) SpecI
  instance GetSetLook DB (Ix X) SpecX
  instance GetSetLook DB (Ix T) DefnT
  instance GetSetLook DB (Ix C) DefnC
  instance GetSetLook DB (Ix O) DefnO
  instance GetSetLook DB (Ix F) DefnF
  instance GetSetLook DB (Ix B) DefnB
  instance GetSetLook DB (Ix I) DefnI
  instance GetSetLook DB (Ix X) DefnX
  instance GetSetLook DB (Ix T) MD
  instance GetSetLook DB (Ix C) MD
  instance GetSetLook DB (Ix O) MD
  instance GetSetLook DB (Ix F) MD
  instance GetSetLook DB (Ix B) MD
  instance GetSetLook DB (Ix I) MD
  instance GetSetLook DB (Ix X) MD
  instance GetSetLook ModDB (Ix M) Mod
  instance GetSetLook SpecDB (Ix T) SpecT
  instance GetSetLook SpecDB (Ix C) SpecC
  instance GetSetLook SpecDB (Ix O) SpecO
  instance GetSetLook SpecDB (Ix F) SpecF
  instance GetSetLook SpecDB (Ix B) SpecB
  instance GetSetLook SpecDB (Ix I) SpecI
  instance GetSetLook SpecDB (Ix X) SpecX
  instance GetSetLook DefnDB (Ix T) DefnT
  instance GetSetLook DefnDB (Ix C) DefnC
  instance GetSetLook DefnDB (Ix O) DefnO
  instance GetSetLook DefnDB (Ix F) DefnF
  instance GetSetLook DefnDB (Ix B) DefnB
  instance GetSetLook DefnDB (Ix I) DefnI
  instance GetSetLook DefnDB (Ix X) DefnX
  -- }}}
  ---------------------
-- }}}

class NamesM m a where namesM :: a -> m N
class CloneM m where cloneM :: N -> m Rn
class RenameM m where renameM :: Rn -> N -> m ()
class CopyInM m where copyInM :: DB -> N -> m ()
class CopyOutM m where copyOutM :: N -> m DB

-----------------------------------------------------------------------------

signature DB_OPS = sig
-- {{{
signature OP = sig
  type Op
  type GenOp
  data OpConfig = OpConfig
    {opConfigAllOps     :: IxSet OP
    ,opConfigAllGrnOps  :: IxSet GENOP
    ,opConfigGenOpInfo  :: IxMap GENOP OpScheme
    ,opConfigOpInfo     :: IxMap OP (Ix GENOP, [Mode], Attrs)
    ,opConfigToOp       :: Map Op (Ix OP)
    ,opConfigFromOp     :: IxMap OP Op} __D
  data Mode = MODE_i | MODE_u | MODE_s | MODE_f __D_BE
  data OpScheme = OpScheme
    {opSchemeNInRange :: (Size, Size) -- ^ hi==0 means unbounded
    ,opSchemeNOutRange :: (Size, Size) -- ^ hi==0 means unbounded
    ,opSchemeNOpandTypes :: Size
    ,opSchemeOpandTypesIn :: [Pos]
    ,opSchemeOpandTypesOut :: [Pos]} __D
  op_ARITY :: [(Op, Int, Int)]
  op_MAYTRAP :: [Op]
  op_COMMUTATIVE :: [Op]
  -- ...
 -- }}}
  cvtInfoByConversion :: [(Conversion, [(Type, Type)])]
  data Conversion = NONE | SX | ZX | CHOP !Int !Int | U2F | F2U | I2F | F2I | F2F __D
  data Round = RTE | RTZ | RTP | RTN __D_BE

  data BUILTIN
  data OP

class
  (OpEval_BinOp a
  ,OpEval_Cvt a
  ,OpEval_Cmp a
  ,OpEval_Select a) => IntCst a;
class OpEval_Composite a where
  extract :: a -> Pos -> a {-b-}
  insert  :: a{-b-} -> Pos -> a -> a
  shuffle :: a -> a -> [Pos] -> a;
class OpEval_BinOp a where
  shl :: a -> a -> a
-- {{{
  lshr :: a -> a -> a
  ashr :: a -> a -> a
  and :: a -> a -> a
  or :: a -> a -> a
  xor :: a -> a -> a
  add :: a -> a -> a
  fadd :: a -> a -> a
  sub :: a -> a -> a
  fsub :: a -> a -> a
  mul :: a -> a -> a
  fmul :: a -> a -> a
  udiv :: a -> a -> a
  sdiv :: a -> a -> a
  fdiv :: a -> a -> a
  urem :: a -> a -> a
  srem :: a -> a -> a
  frem :: a -> a -> a;
class OpEval_Cvt a where
  trunc :: a -> a -> a
  zext :: a -> a -> a
  sext :: a -> a -> a
  fptoui :: a -> a -> a
  fptosi :: a -> a -> a
  uitofp :: a -> a -> a
  sitofp :: a -> a -> a
  fptrunc :: a -> a -> a
  fpext :: a -> a -> a
  ptrtoint :: a -> a -> a
  inttoptr :: a -> a -> a
  bitcast :: a -> a -> a
  addrspacecast :: a -> a -> a;
class OpEval_CmpOp a where
  cmp :: CMP_OP -> a -> a -> a;
class OpEval_Select a where
  select :: a -> a -> a -> a;
-- }}}

-----------------------------------------------------------------------------

signature DB_MM_1 = sig
  -------------------------
  type DefnI
  -------------------------
  data SpecB = SpecB {sbAttrs :: Attrs}
  data SpecI = SpecI {siAttrs :: Attrs}
  data SpecX = SpecX {sxAttrs :: Attrs, sxType :: !(Ix TYPE)}
  -------------------------
  type DefnB = Block
  data DefnX = DefnX {dxPort :: !(Port I)}
  -------------------------
  data CFGInfo = CFGInfo
    {cfgInfoUD    :: UD
    ,cfgInfoRefs  :: N ()
    ,cfgInfoGraph :: GraphInfo
    ,cfgInfoDom   :: DomInfo
    } __D
  data UD = UD
    {
    } __D
  data GraphInfo = GraphInfo
    {graphInfoPreds             :: Graph B
    ,graphInfoSuccs             :: Graph B
    ,graphInfoAugPreds          :: Graph B
    ,graphInfoAugSuccs          :: Graph B
    ,graphInfoBlockSCC          :: SCC B
    ,graphInfoUnreachableRoots  :: IxMap (SCC B) ()
    ,graphInfoUnescapableLeaves :: IxMap (SCC B) ()
    ,graphInfoFakeEntryEdges    :: IxMap B ()
    ,graphInfoFakeExitEdges     :: IxMap B ()
    } __D
  data DomInfo = DomInfo
    {domInfoDom :: DomData
    ,domInfoPDom :: DomData
    } __D
  data DomData = DomData
    {domDataTree :: DomTree B
    ,domDataFront :: DomFront B
    ,domDataFrontPlus :: DomFrontPlus B
    } __D
  -------------------------
  data EDGE
  data EDGES
  data CFG = CFG
    {cfgNIns    :: !Size
    ,cfgNOuts   :: !Size
    ,cfgNObjs   :: !Size
    ,cfgNExits  :: !Size
    ,cfgNBlocks :: !Size
    ,cfgIns     :: PosMap (Ix X)
    ,cfgOuts    :: PosMap (Ix X)
    ,cfgObjs    :: IxMap OBJ ()
    ,cfgEntry   :: !(Ix B)
    ,cfgExit    :: !(Ix B) -- ^ @==0@ if not exactly @1@ exit (???)
    ,cfgExits   :: IxMap B ()
    ,cfgBlocks  :: IxMap B ()
    ,cfgEdge    :: IxMap EDGE Edge
    ,cfgEdges   :: IxMap EDGES Edges
    } __D
  data Block = Block
    {blockInsns :: BlockInsns
    ,blockInfo :: BlockInfo
    } __D
  type VEC = [] -- XXX
  data BlockInsns = BlockInsns
    {blockInsnsN :: !Size
    ,blockInsnsNPhi :: !Size
    ,blockInsnsNSgm :: !Size
    ,blockInsnsNMid :: !Size
    ,blockInsnsPhi :: IxMap I ()
    ,blockInsnsSgm :: IxMap I ()
    ,blockInsnsMid :: VEC(Ix I)
    ,blockInsnsLast :: !(Ix I)
    } __D
  data BlockInfo = BlockInfo
    {blockInfoIn :: BlockIn
    ,blockInfoOut :: BlockOut
    ,blockInfoLast :: BlockLast
    } __D
  data BlockIn = BlockIn
    {blockInN     :: !Size
    ,blockInPhiI  :: IxMap I ()
    ,blockInPhiP  :: PosMap (IxMap X (Ix X))
    ,blockInB     :: PosMap (Ix B)
    ,blockInE     :: PosMap (Ix EDGE)
    } __D
  -- | If there is a default target, it is at position @0@, and
  -- @not(IM.member 0 (IM.fromList (M.elems blockOutV)))@.
  data BlockOut = BlockOut
    {blockOutN    :: !Size
    ,blockOutSgmI :: IxMap I ()
    ,blockOutSgmP :: PosMap (IxMap X (Ix X))
    ,blockOutB    :: PosMap (Ix B)
    ,blockOutE    :: PosMap (Ix EDGE)
    ,blockOutV    :: PosMap EdgeVal
    ,blockOutW    :: PosMap EdgeWeight
    } __D
  data BlockLast = BlockOut
    {blockLastOp :: !(Ix OP)
    ,blockLastI :: !(Ix I)
    ,blockLastX :: Maybe (Ix X)
    ,blockLastV :: EdgeValMap Pos
    } __D
  type EdgeVal = NumLit
  type EdgeValMap = Map NumLit
  type EdgeValSet = EdgeValMap ()
  type EdgeWeight = WORD
  data Edge = Edge
    {edgeEdges :: !(Ix EDGES)
    ,edgeSrc :: !(Port BLOCK)
    ,edgeTgt :: !(Port BLOCK)
    } __D
  data Edges = Edges
    {edgesN     :: !Size
    ,edgesSrc   :: !(Ix B)
    ,edgesTgt   :: !(Ix B)
    ,edgesPos   :: PosMap Pos -- ^ (psrc,ptgt)
    ,edgesEdgeI :: IxMap EDGE Pos -- ^ (e,psrc)
    ,edgesEdgeP :: PosMap (Ix EDGE) -- ^ (psrc,e)
    } __D
  --newtype PhiMat = PhiMat PhiSgmMat
  --newtype SgmMat = SgmMat PhiSgmMat
  --newtype PhiCol = PhiCol PhiSgmCol
  --newtype SgmCol = SgmCol PhiSgmCol
  --type PhiSgmMat = IxMap ID (PosMap (Ix ID))
  --type PhiSgmRow = (Ix ID, PosMap (Ix ID))
  --type PhiSgmCol = IxMap ID (Ix ID)
  --type PhiArg = (Ix ID, Ix BLOCK)
  --type SgmRes = (Ix ID, Ix BLOCK)
  --type PhiRes = Ix ID
  --type SgmArg = Ix ID

-----------------------------------------------------------------------------

-- |
-- {{{
-- > * Styles of phi/sigma instructions:
-- >     1) UNRESTRICTED
-- >         * [BLOCKS]
-- >             1) multiple args/results from/to same block REQUIRED for multiedges.
-- >         * [VARS]
-- >             1) different vars in args/results from/to same block ALLOWED.
-- >     2) SAME_BLOCK_SAME_VAR
-- >         * [BLOCKS]
-- >             1) As for [UNRESTRICTED-BLOCKS].
-- >         * [VARS]
-- >             1) ALL vars in args/results from/to same block REQUIRED to be the SAME.
-- >     3) ONE_EDGE_PER_BLOCK
-- >         * [BLOCKS]
-- >             1) AT MOST a SINGLE arg/result from/to each block ALLOWED.
-- >         * [VARS]
-- >             1) As for [SAME_BLOCK_SAME_VAR-VARS], trivially.
-- > * Who uses which styles (phi,sgm):
-- >     * (SAME_BLOCK_SAME_VAR,<no-sigmas>)
-- >         1) LLVM
-- >     * (ONE_EDGE_PER_BLOCK,<no-sigmas>
-- >         1) SPIR-V
-- }}}
class (CFGBase a, CFGInfo a, CFGModify a) => CFG a
class CFGBase cfg where
  cfgDefd     :: cfg -> N
  cfgRefd     :: cfg -> N
  cfgInputs   :: cfg -> PosMap (Ix X)
  cfgOutputs  :: cfg -> PosMap (Ix X)
  cfgEntry    :: cfg -> Ix B
  cfgExit     :: cfg -> Ix B
  blockNIn    :: cfg -> Ix B -> Size
  blockNOut   :: cfg -> Ix B -> Size
  blockIn     :: cfg -> Ix B -> BlockIn
  blockOut    :: cfg -> Ix B -> BlockOut
  blockPhis   :: cfg -> Ix B -> PhiMat
  blockSgms   :: cfg -> Ix B -> SgmMat
  blockLast   :: cfg -> Ix B -> BlockLast
  blockInsns  :: cfg -> Ix B -> [Ix I]
  blockSuccs  :: cfg -> Ix B -> IxMap B ()
  blockPreds  :: cfg -> Ix B -> IxMap B ()
  blockSuccG  :: cfg -> IxMap B (IxMap B ())
  blockPredG  :: cfg -> IxMap B (IxMap B ())
class CFGModify cfg where
  insertEdge   :: cfg -> Ix Ix B -> SgmCol -> Pos -> Ix B -> PhiCol -> (Pos, cfg)
  insertEdges  :: cfg -> Ix Ix B -> SgmMat -> PosMap (Ix B, PhiCol) -> (PosMap Pos, cfg)
  deleteEdge   :: cfg -> Ix Ix B -> Pos -> Ix B -> Pos -> cfg
  insertBlock  :: cfg -> Ix Ix B -> cfg
  deleteBlock  :: cfg -> Ix Ix B -> cfg
-- | Assumes /NOT/ [ONE_EDGE_PER_BLOCK]-style phis/sigmas.
class CFGInfo cfg where
  edgeSrcTgt   :: cfg -> Ix B -> Ix B -> (PosMap Pos, PosMap Pos)
  edgeTgt      :: cfg -> Ix B -> Pos -> (Port B) -- ^ src/src pos/tgt port
  edgeSrc      :: cfg -> Ix B -> Pos -> (Port B) -- ^ tgt/tgt pos/src port
  phiMat       :: cfg -> Ix B -> PhiMat
  sgmMat       :: cfg -> Ix B -> SgmMat
  phiCol       :: cfg -> Ix B -> Pos -> PhiCol -- ^ tgt/tgt pos/[(phi result, phi arg)]
  sgmCol       :: cfg -> Ix B -> Pos -> SgmCol -- ^ src/src pos/[(sgm arg, sgm result)]
  phiVar       :: cfg -> Ix B -> Pos -> Ix X -> Ix X -- ^ tgt/tgt pos/phi result/phi arg
  sgmVar       :: cfg -> Ix B -> Pos -> Ix X -> Ix X -- ^ src/src pos/sgm arg/sgm result
  phiVarFrom   :: cfg -> Ix B -> Pos -> Ix X -> (Ix X, Port B) -- ^ src/src pos/phi result in tgt/(phi arg in tgt,  tgt port)
  sgmVarTo     :: cfg -> Ix B -> Pos -> Ix X -> (Ix X, Port B) -- ^ tgt/tgt pos/sgm arg in src/(sgm result in src, src port)
  ---------------------------------------------------------------
  phiVar o tgt tgtpos var | PhiCol col <- phiCol o tgt tgtpos = col Ix.! var
  sgmVar o src srcpos var | SgmCol col <- sgmCol o src srcpos = col Ix.! var
  phiVarFrom o src srcpos var | a@(Port tgt tgtpos) <- edgeTgt o src srcpos, x <- phiVar o tgt tgtpos var = (x, a)
  sgmVarTo o tgt tgtpos var | a@(Port src srcpos) <- edgeSrc o tgt tgtpos, x <- sgmVar o src srcpos var = (x, a)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

signature COPY = sig
  class Copy from to roots rename where
    copy :: rename -> from -> to -> roots -> (rename, to, from)
  class CopyM m roots rename where
    copyM :: rename -> roots -> m rename
  class CopyOutM m to roots rename where
    copyOutM :: rename -> to -> roots -> m (rename, to)
  class CopyInM m from roots rename where
    copyInM :: rename -> from -> roots -> m (rename, from)

signature GRAPHOPS
  class RefsM m set a where
    refsM :: a -> m set
  class ReachM m set where
    reachM :: set -> m set

signature OUTOFDATEM = sig
  class (Monad m) => OutOfDateM m a where
    getDirtyM :: m (IxSet a)
    popDirtyM :: m (Maybe (Ix a))
    getIndsM :: Ix a -> m (IxSet a)
    setIndsM :: Ix a -> IxSet a -> m ()
    clearIndsM :: Ix a -> m ()
    --getAndClearIndsM :: Ix a -> m (IxSet a)
    --mapIndsM :: (IxSet a -> IxSet a) -> Ix a -> m ()
    --getDirtyM = return mempty
    --popDirtyM = return Nothing
    --getIndsM _ = return mempty
    --setIndsM _ _ = return ()
    --clearIndsM i = setIndsM i mempty
    --getAndClearIndsM i = do
    --  inds <- getIndsM i
    --  clearIndsM i
    --  return inds
    --mapIndsM f i = do
    --  inds <- getIndsM i
    --  let !inds2 = f inds
    --  setIndsM i inds2

#if 0
supportM :: m (IxMap a (Ix a))
collectM :: m (IxMap a (Ix a))
quotientM :: m (IxMap a (Ix a), IxMap a b)
compressAndIndexM :: m (IxMap a (Ix a))
supportAndSizeM :: m (Int, IxMap a (Ix a))
collectAndReindexFromM :: Int -> m (Int, IxMap a (Ix a))
quotientAndReindexFromM :: Int -> m (Int, IxMap a (Ix a), IxMap a b)
#endif

signature UNIONFINDM = sig
  class (FindM m a b, UnionMapM m a b) => UFM m a b
  class (Monad m) => IndM m a where
    indM :: Ix a -> Ix a -> m ()
  class (Monad m) => RepM m a where
    repM :: Ix a -> m (Ix a)
    tryRepM :: Ix a -> m (Maybe (Ix a))
    tryRepM i = do {j <- repM i; return (Just j)}
  class (IndM m a) => UnionM m a where
    unionM :: Ix a -> Ix a -> m (Ix a)
    unionToLeftM :: Ix a -> Ix a -> m (Ix a)
    unionM_ :: Ix a -> Ix a -> m ()
    unionToLeftM_ :: Ix a -> Ix a -> m ()
    unionM_ i j = unionM i j >> return ()
    unionToLeftM_ i j = unionToLeftM i j >> return ()
    unionToLeftM i j = indM j i >> return i
  class (RepM m a) => FindM m a b where
    findM :: Ix a -> m (Ix a, b)
    tryFindM :: Ix a -> m (Maybe (Ix a, b))
    tryFindM i = do
      o <- tryRepM i
      case o of
        Just i-> do
          x <- findM i
          return (Just x)
        Nothing-> return Nothing
  class (UnionM m a) => UnionMapM m a b where
    unionAndSetM :: Ix a -> Ix a -> b -> m (Ix a)
    unionToLeftAndSetM :: Ix a -> Ix a -> b -> m (Ix a)
    unionWithM :: (b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
    unionWithRepM :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
    unionToLeftWithM :: (b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
    unionToLeftWithRepM :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
    unionAndSetM_ :: Ix a -> Ix a -> b -> m ()
    unionToLeftAndSetM_ :: Ix a -> Ix a -> b -> m ()
    unionWithM_ :: (b -> b -> b) -> Ix a -> Ix a -> m ()
    unionWithRepM_ :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m ()
    unionToLeftWithM_ :: (b -> b -> b) -> Ix a -> Ix a -> m ()
    unionToLeftWithRepM_ :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m ()
    unionAndSetM_ i j a = unionAndSetM i j a >> return ()
    unionToLeftAndSetM_ i j a = unionToLeftAndSetM i j a >> return ()
    unionWithM_ f i j = unionWithM f i j >> return ()
    unionWithRepM_ f i j = unionWithRepM f i j >> return ()
    unionToLeftWithM_ f i j = unionToLeftWithM f i j >> return ()
    unionToLeftWithRepM_ f i j = unionToLeftWithRepM f i j >> return ()

-----------------------------------------------------------------------------
```

## Three

```haskell
{-# OPTIONS -pgmP cpp -optP-w #-}
{-# LANGUAGE
      CPP,
      MagicHash,
      BangPatterns,
      OverlappingInstances,
      UndecidableInstances,
      IncoherentInstances #-}
#define __D deriving(Eq,Ord,Read,Show)
#define __D_BE deriving(Eq,Ord,Read,Show,Bounded,Enum)
#ifdef __DEBUG_ON
#define __IF_DEBUG(on,off) on
#else
#define __IF_DEBUG(on,off) off
#endif
#define __DEBUG(o)  __IF_DEBUG(o,)
#define __PANIC(what,note)\
  (error(concat[__FILE__,":",show(__LINE__),":[",(what),"]:",(note)]))
#define __BUG(note)     __PANIC("BUG",note)
#define __FIXME(note)   __PANIC("FIXME",note)
#define __IMPOSSIBLE    __BUG("impossible")
#define __ASSERT(cond)  __IF_DEBUG(__ASSERT_DO(cond),True)
#define __ASSERT_FAIL(note) __PANIC("ASSERT",note)
#define __ASSERT_DO(cond)\
  (if (cond) then True else (__ASSERT_FAIL(#cond)))
#define HASH #
#define __INLINE(x) {-HASH INLINE x HASH-}

module PartRefineUnionFind where

import Prelude hiding(null)
import qualified Prelude as P
import Data.Int
import Data.Word
import Data.Bits
import MM.Data.Map.Ord(Map)
import MM.Data.Set.Ord(Set)
import MM.Data.Map.Int(IntMap)
import MM.Data.Set.Int(IntSet)
import qualified MM.Data.Map.Ord as OM
import qualified MM.Data.Set.Ord as OS
import qualified MM.Data.Map.Int as IM
import qualified MM.Data.Set.Int as IS
import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..))
import Control.Monad
import Data.Function
import Data.List(foldl')
import MM.Data.Tree.Rose(Tree(..))
import qualified MM.Control.Monad.S.U as SU
import qualified MM.Control.Monad.S.U2 as SU2
import qualified MM.Control.Monad.Class as N
import MM.Data.Types.Ix
import MM.Data.Map.Ix(IxMap)
import MM.Data.Set.Ix(IxSet)
import MM.Data.Trie.Ix(IxTrie)
import qualified MM.Data.Map.Ix as Ix
import qualified MM.Data.Set.Ix as IxS
import qualified MM.Data.Trie.Ix as IxT
import MM.Data.UnionFind.Ix(UF)
import qualified MM.Data.UnionFind.Ix as U
import qualified MM.Data.Class.UnionFind.Ix as UF
import MM.Data.Class.Base
import qualified MM.Data.Class.Maps as M

-----------------------------------------------------------------------------

#if 0
derivedRename
  :: IxMap a (Ix a)
  -> IxMap a (Ix b)
  -> IxMap b (Ix b)
derivedRename a2a a2b
  = Ix.fromList . Ix.elems
  $ Ix.intersectionWith go a2b a2a
  where go b1 a2 = (b1, a2b Ix.! a2)
#endif

shareSingleOutputCyclic
  :: (Ord k)
  => IxMap a (k, [Ix a])
  -> (IxMap a (Ix a), PartRefineQ a)
shareSingleOutputCyclic dfa
  | init <- partRefinePrep dfa
  = partRefine init
shareMultiOutputCyclic
  :: (Ord k)
  => IxMap a (k, [Ix b], [Ix b])
  -> ((IxMap a (Ix a), IxMap b (Ix b)), PartRefineQ c)
shareMultiOutputCyclic circ
  | dfa <- multiToSingleOutput circ
  , (c2c, o) <- shareSingleOutputCyclic dfa
  , rns <- singleToMultiRename circ c2c
  = (rns, o)
shareMultiOutputCyclicWithShift
  :: (Ord k)
  => Ix a
  -> IxMap a (k, [Ix b], [Ix b])
  -> ((IxMap a (Ix a), IxMap b (Ix b)), PartRefineQ c)
shareMultiOutputCyclicWithShift shift circ
  | (dfa,bs) <- multiToSingleOutputWithShift shift circ
  , (c2c, o) <- shareSingleOutputCyclic dfa
  , rns <- singleToMultiRenameWithShift shift bs c2c
  = (rns, o)

multiToSingleOutput
  :: IxMap a (k, [Ix b], [Ix b])
  -> IxMap c (Either EdgeLbl k, [Ix c])
multiToSingleOutput = Ix.foldWithKey go mempty
  where go i (k,js,ks) o
          | xi <- castIx i :: Ix c
          , one <- Ix.singleton xi (Right k,castIxArg ks)
          , two <- Ix.fromList $ zipWith
              (\xj p->(xj,(Left p,[xi])))
              (castIxArg js) [0..]
          = o`Ix.union`one`Ix.union`two
multiToSingleOutputWithShift
  :: Ix a
  -> IxMap a (k, [Ix b], [Ix b])
  -> (IxMap c (Either EdgeLbl k, [Ix c]), IxMap b ())
multiToSingleOutputWithShift shift = Ix.foldWithKey go mempty
  where go i (k,js,ks) (o,bs)
          | xi <- castIx (i + shift) :: Ix c
          , one <- Ix.singleton xi (Right k,castIxArg ks)
          , two <- Ix.fromList $ zipWith
              (\xj p->(xj,(Left p,[xi])))
              (castIxArg js) [0..]
          , !o <- o`Ix.union`one`Ix.union`two
          , !bs <- bs`Ix.union`Ix.fromList (zip (js ++ ks) (repeat ()))
          = (o,bs)

singleToMultiRename
  :: IxMap a dontcare
  -> IxMap c (Ix c)
  -> (IxMap a (Ix a), IxMap b (Ix b))
singleToMultiRename as c2c
  | b2b <- cast (cast c2c `Ix.difference` as)
  , a2a <- cast c2c `Ix.intersection` as
  = (a2a, b2b)
  where cast :: IxMap x (Ix x) -> IxMap y (Ix y)
        cast = castIxArg . Ix.castIxMap
singleToMultiRenameWithShift
  :: Ix a
  -> IxMap b dontcare
  -> IxMap c (Ix c)
  -> (IxMap a (Ix a), IxMap b (Ix b))
singleToMultiRenameWithShift shift bs c2c
  | b2b <- cast c2c `Ix.intersection` bs
  , a2a <- cast (cast c2c `Ix.difference` bs)
  , a2a <- Ix.fromList [(i - shift, j - shift) | (i,j) <- Ix.toList a2a]
  = (a2a, b2b)
  where cast :: IxMap x (Ix x) -> IxMap y (Ix y)
        cast = castIxArg . Ix.castIxMap

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data SET a
type EdgeLbl = Int
type EdgeLblMap = IntMap
type EdgeLblSet = EdgeLblMap ()

class DeltaInv x a | x -> a where
  deltaInv :: x -> EdgeLbl -> IxMap a () -> IxMap a ()
  edgeLabels :: x -> EdgeLblSet
  updateDeltaInvL :: x -> Ix a -> [Ix a] -> x
  updateDeltaInvP :: x -> Ix a -> EdgeLblMap (Ix a) -> x
  updateDeltaInvL x i is = updateDeltaInvP x i (IM.fromList (zip [0..] is))

class Partition x a | x -> a where
  suppOfPart :: x -> IxMap a (Ix a)
  splitPart :: x -> Ix (SET a) -> IxMap a () -> IxMap a () -> (IxMap a (), x)
  insertSingles :: x -> IxMap a () -> x
  insertClass :: x -> IxMap a () -> x
  repOfPart :: x -> Ix a -> Ix (SET a)
  setOfPart :: x -> Ix (SET a) -> IxMap a ()
  sizeOfPart :: x -> Ix (SET a) -> Int
  setsOfPart :: x -> [IxMap a ()]

partRefine :: forall x a. (Partition x a, DeltaInv x a) => x -> (IxMap a (Ix a), x)
partRefine x
  | ls <- setsOfPart x
  , x <- go (x, ls)
  = (suppOfPart x, x)
  where elbls = edgeLabels x
        fold l s = IM.foldWithKey (\elbl _ s@(x,_)->
            step s (deltaInv x elbl l)
          ) s elbls
        go :: (x, [IxMap a ()]) -> x
        go (ps,[]) = ps
        go (ps,l:ls) = go (fold l (ps,ls))
        step :: (x, [IxMap a ()]) -> IxMap a () -> (x, [IxMap a ()])
        step s a = go s a
          where go :: (x, [IxMap a ()]) -> IxMap a () -> (x, [IxMap a ()])
                go s@(x,_) a
                  | Ix.null a = s
                  | i <- minKey a
                  , cls <- repOfPart x i
                  , (snew, anew) <- refineOne s cls a
                  = go snew anew
                minKey m
                  | Just ((k,_),_) <- Ix.minViewWithKey m = k
                  | otherwise = __IMPOSSIBLE
                refineOne
                  :: (x, [IxMap a ()])
                  -> Ix (SET a) -> IxMap a ()
                  -> ((x, [IxMap a ()]), IxMap a ())
                refineOne s@(x,ls) cls dinv
                  | p <- setOfPart x cls
                  , p1 <- p/\dinv
                  , p2 <- p\\p1
                  , xdinv <- dinv\\p -- this is an optimization
                  , o1 <- Ix.null p1
                  , o2 <- Ix.null p2
                  = case (o1,o2) of
                      (True,True)-> __IMPOSSIBLE
                      (True,False) | __ASSERT(p == p2)-> (s, xdinv)
                      (False,True) | __ASSERT(p == p1)-> (s, xdinv)
                      (False,False)
                        | (p0, x) <- splitPart x cls p1 p2
                        -> ((x, p0:ls), xdinv)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class PartRefinePrep x a where
  partRefinePrep :: x -> PartRefineQ a
instance PartRefinePrep (PartRefineQ a) a where
  partRefinePrep = id
instance (Ord k) => PartRefinePrep (IxMap a (k, EdgeLblMap (Ix a))) a where
  partRefinePrep dfa
    | (dinv,kpart) <- Ix.foldWithKey go mempty dfa
    , part <- foldl' insertClass mempty (OM.elems kpart)
    = PRQ part dinv
    where go i (key,js) (dinv,part)
            | part <- OM.insertWith (\/) key (Ix.singleton i ()) part
            , dinv <- updateDeltaInvP dinv i js
            = (dinv,part)
instance (Ord k) => PartRefinePrep (IxMap a (k, [Ix a])) a where
  partRefinePrep dfa
    | (dinv,kpart) <- Ix.foldWithKey go mempty dfa
    , part <- foldl' insertClass mempty (OM.elems kpart)
    = PRQ part dinv
    where go i (key,js) (dinv,part)
            | part <- OM.insertWith (\/) key (Ix.singleton i ()) part
            , dinv <- updateDeltaInvL dinv i js
            = (dinv,part)
data NodeKey k = UniqKey | NodeKey k __D
data MyNodeKey a k = MyUniqKey !(Ix a) | MyNodeKey k deriving(Eq,Ord,Read,Show)
instance (Ord k) => PartRefinePrep (IxMap a (NodeKey k, [Ix a])) a where
  partRefinePrep dfa
    | (dinv,kpart) <- Ix.foldWithKey go mempty dfa
    , part <- foldl' insertClass mempty (OM.elems kpart)
    = PRQ part dinv
    where go i (key,js) (dinv,part)
            | newkey <- case key of
                UniqKey-> MyUniqKey i
                NodeKey k-> MyNodeKey k
            , part <- OM.insertWith (\/) newkey (Ix.singleton i ()) part
            , dinv <- updateDeltaInvL dinv i js
            = (dinv,part)

data PartRefineQ a = PRQ
  {prqPartition :: PartitionQ a
  ,prqDeltaInv :: DeltaInvQ a} __D
data PartitionQ a = PQ
  {pqNext :: !(Ix (SET a))
  ,pqRep :: IxMap a (Ix (SET a))
  ,pqSingle :: IxMap (SET a) (Ix a)
  ,pqMulti :: IxMap (SET a) (PQNode a)
  } __D
data PQNode a = PQNode
  {pqNodeElts :: IxMap a ()
  ,pqNodeSize :: !Int} __D
data DeltaInvQ a = DIQ
  {diqMemo :: UF a (DIQNode a)
  ,diqDeltaInv :: EdgeLblMap (IxMap a (IxMap a ()))} __D
data DIQNode a = DIQNode
  {diqNodeTgtLocs :: EdgeLblSet
  ,diqNodeSrcLocs :: EdgeLblMap (IxMap a ())} __D
-- {{{
instance Monoid (PartRefineQ a) where
  mempty = PRQ mempty mempty
  mappend (PRQ a1 b1) (PRQ a2 b2)
    | !a <- mappend a1 a2
    , !b <- mappend b1 b2
    = PRQ a b
instance Monoid (DeltaInvQ a) where
  mempty = DIQ mempty mempty
  mappend = __FIXME("mappend<DeltaInvQ>")
instance Monoid (PartitionQ a) where
  mempty = PQ
    {pqNext=1
    ,pqRep=mempty
    ,pqSingle=mempty
    ,pqMulti=mempty}
  mappend = __FIXME("mappend<PartitionQ>")
-- }}}

instance Partition (PartRefineQ a) a where
  suppOfPart PRQ{..} = suppOfPart prqPartition
  splitPart PRQ{..} a b c | (o,prqPartition) <- splitPart prqPartition a b c = (o,PRQ{..})
  insertSingles PRQ{..} a | prqPartition <- insertSingles prqPartition a = PRQ{..}
  insertClass PRQ{..} a | prqPartition <- insertClass prqPartition a = PRQ{..}
  repOfPart PRQ{..} a = repOfPart prqPartition a
  setOfPart PRQ{..} a = setOfPart prqPartition a
  sizeOfPart PRQ{..} a = sizeOfPart prqPartition a
  setsOfPart PRQ{..} = setsOfPart prqPartition

instance Partition (PartitionQ a) a where
  insertSingles PQ{..} new
    | n <- Ix.size new
    , newsingles <- Ix.fromList $ zip [pqNext..pqNext+Ix n-1] (Ix.keys new)
    , newreps <- Ix.fromList $ [(b,a) | (a,b) <- Ix.toList newsingles]
    , pqNext <- pqNext + Ix n
    , pqRep <- pqRep`Ix.union`newreps
    , pqSingle <- pqSingle`Ix.union`newsingles
    = PQ{..}
  insertClass x@PQ{..} new
    | size <- Ix.size new
    = case compare size 1 of
        LT-> x
        EQ-> insertSingles x new
        GT
          | rep <- pqNext
          , pqNext <- pqNext + 1
          , pqRep <- pqRep`Ix.union`fmap (const rep) new
          , node <- PQNode
              {pqNodeElts=new
              ,pqNodeSize=size}
          , pqMulti <- Ix.insert rep node pqMulti
          -> PQ{..}
  repOfPart PQ{..} i = Ix.findWithDefault 0 i pqRep
  setOfPart PQ{..} cls
    | Just PQNode{..} <- Ix.lookup cls pqMulti
    = pqNodeElts
    | Just i <- Ix.lookup cls pqSingle
    = Ix.singleton i ()
    | otherwise
    = mempty
  sizeOfPart PQ{..} cls
    | Just PQNode{..} <- Ix.lookup cls pqMulti
    = pqNodeSize
    | Just i <- Ix.lookup cls pqSingle
    = 1
    | otherwise
    = 0
  setsOfPart PQ{..}
    | o <- fmap (\i-> Ix.singleton i ()) pqSingle
    , o <- Ix.foldWithKey (\cls PQNode{..}-> Ix.insert cls pqNodeElts) o pqMulti
    = Ix.elems o
  suppOfPart PQ{..} = Ix.foldWithKey go mempty pqMulti
    where go _ PQNode{..} o
            | rep <- Ix.minKeyWithDefault 0 pqNodeElts
            , new <- fmap (const rep) (Ix.delete rep pqNodeElts)
            = o`Ix.union`new
  splitPart x@PQ{..} cls p1 p2
    -- Splits the smaller of the two sets into a new class, and
    -- returns the smaller one. It MUST be the case and is UNCHECKED
    -- that the two sets are NONMEMPTY. And it MUST be the case and
    -- is UNCHECKED that the two sets form a partition of the class
    -- identified by the @Int@.
    | n1 <- Ix.size p1 -- XXX: O(n)
    , n2 <- sizeOfPart x cls - n1
    , __ASSERT(n1/=0 && n2/=0)
    , let go q1 q2 m1 m2
            | !new <- pqNext
            , !pqNext <- pqNext + 1
            , !pqRep <- fmap (const new) q1`Ix.union`pqRep
            = case (m1==1,m2==1) of
                (True,True)
                  | i1 <- Ix.minKeyWithDefault 0 q1
                  , i2 <- Ix.minKeyWithDefault 0 q2
                  , pqSingle <- Ix.insert new i1 pqSingle
                  , pqSingle <- Ix.insert cls i2 pqSingle
                  , pqMulti <- Ix.delete cls pqMulti
                  -> PQ{..}
                (True,False)
                  | i1 <- Ix.minKeyWithDefault 0 q1
                  , pqSingle <- Ix.insert new i1 pqSingle
                  , node2 <- PQNode{pqNodeElts=q2,pqNodeSize=m2}
                  , pqMulti <- Ix.insert cls node2 pqMulti
                  -> PQ{..}
                (False,True)-> __IMPOSSIBLE
                (False,False)
                  | node1 <- PQNode{pqNodeElts=q1,pqNodeSize=m1}
                  , node2 <- PQNode{pqNodeElts=q2,pqNodeSize=m2}
                  , pqMulti <- Ix.insert new node1 pqMulti
                  , pqMulti <- Ix.insert cls node2 pqMulti
                  -> PQ{..}
    = case n1 <= n2 of
        True  | !x <- go p1 p2 n1 n2-> (p1, x)
        False | !x <- go p2 p1 n2 n1-> (p2, x)

instance DeltaInv (PartRefineQ a) a where
  edgeLabels PRQ{..} = edgeLabels prqDeltaInv
  deltaInv PRQ{..} pos tgts = deltaInv prqDeltaInv pos tgts
  updateDeltaInvL PRQ{..} i js
    | !prqDeltaInv <- updateDeltaInvL prqDeltaInv i js
    = PRQ{..}
  updateDeltaInvP PRQ{..} i js
    | !prqDeltaInv <- updateDeltaInvP prqDeltaInv i js
    = PRQ{..}

instance DeltaInv (DeltaInvQ a) a where
  edgeLabels DIQ{..} = fmap (const mempty) diqDeltaInv
  deltaInv DIQ{..} pos tgts = Ix.fold (\/) mempty
    ((IM.findWithDefault mempty pos diqDeltaInv) `Ix.intersection` tgts)
  updateDeltaInvP DIQ{..} i js
    | !diqDeltaInv <- IM.foldWithKey go diqDeltaInv js
    = DIQ{..}
    where !iset = Ix.singleton i ()
          go pos j dinv = IM.insertWith (\/)
            pos (Ix.singleton j iset) dinv

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

#if 0
class Ind x a | x -> a where
  ind :: x -> Ix a -> Ix a -> x
class Uses x a | x -> a where
  usesI :: x -> Ix a -> (IxMap a PosSet, x)
  usesP :: x -> Ix a -> (PosMap (IxMap a ()), x)

data Env a b = Env
  {envUF :: UF a b
  ,envPR :: PartRefineQ a
  ,envU :: UsesQ a}

instance Ind (DeltaInvQ a) a where
  ind DIQ{..} i j
    -- ............
    -- ............
    -- ............
    = undefined

instance PartRefine (Env a b) a where
  -- ...
  -- ...
instance OutOfDate (Env a b) a where
  -- ...
  -- ...
instance U.UF (Env a b) a b where
  -- ...
  -- ...
instance Ind Env NODE where
  ind Env{..} i j
    | envUF <- ind envUF i j
    , envPR <- ind envPR i j
    , envU <- ind envU i j
    = Env{..}
instance Uses Env NODE where
  usesI Env{..} i
    | (o,envU) <- usesI envU i
    = (o,Env{..})
  usesP Env{..} i
    | (o,envU) <- usesP envU i
    = (o,Env{..})

instance Ind (PartRefineQ a) a where
  ind PRQ{..} i j
    | !prqPartition <- ind prqPartition i j
    , !prqDeltaInv <- ind prqDeltaInv i j
    = PRQ{..}
instance Ind (PartitionQ a) a where
  ind PQ{..} i j
    | pqSingle <- Ix.delete i pqSingle
    , pqMulti <- ind pqMulti i j
    = PQ{..}

newtype UsesQ a = UsesQ
  {uqUF :: UF a (UQNode a)}
data UQNode a = UQNode
  {uqNodeP :: PosMap (IxMap a ())
  ,uqNodeI :: IxMap a PosSet}
instance Uses (UsesQ a) a where
  usesI UQ{..} i
    | (UQNode{..},uqUF) <- U.get uqUF i
    = (uqNodeI,UQ{..})
  usesP UQ{..} i
    | (UQNode{..},uqUF) <- U.get uqUF i
    = (uqNodeP,UQ{..})
#endif

-----------------------------------------------------------------------------
```

## Four

```haskell
#if 0
OpCapability*
OpExtension*
OpExtInstImport*
OpMemoryModel
OpEntryPoint*
OpExecutionMode*
-- no forward references
(OpString|OpSourceExtension|OpSource|OpSourceContinued)*
(OpName|OpMemberName)*
(
  -- All (OpDecorate|OpDecorateId|OpMemberDecorate) instructions targeting
  -- an OpDecorationGroup instruction must precede it.
  OpDecorate|
  OpDecorateId|
  OpMemberDecorate|
  OpDecorationGroup|
  OpGroupDecorate|
  OpGroupMemberDecorate
)*
(
  OpTypeForwardPointer|OpTypeVoid|OpTypeBool|OpTypeInt|OpTypeFloat|OpTypeVector|OpTypeMatrix|OpTypeImage|OpTypeSampler|OpTypeSampledImage|OpTypeArray|OpTypeRuntimeArray|OpTypeStruct|OpTypeOpaque|OpTypePointer|OpTypeFunction|OpTypeEvent|OpTypeDeviceEvent|OpTypeReserveId|OpTypeQueue|OpTypePipe|OpTypeNamedBarrier|OpTypePipeStorage|
  OpUndef|OpConstantTrue|OpConstantFalse|OpConstant|OpConstantComposite|OpConstantSampler|OpConstantNull|OpSpecConstantTrue|OpSpecConstantFalse|OpSpecConstant|OpSpecConstantComposite|OpSpecConstantOp|
  <global-OpVariable>
)*
(
  OpFunction
  OpFunctionParameter*
  OpFunctionEnd
)*
(
  OpFunction
  OpFunctionParameter*
  (
    OpLabel
    OpVariable*
    <code-non-terminator>*
    <code-terminator>
  )
  (
    OpLabel
    <code-non-terminator>*
    <code-terminator>
  )*
  OpFunctionEnd
)*
#endif

#if 0
ModInfo   OpCapability*
ModInfo   OpExtension*
INSNS     OpExtInstImport*
ModInfo   OpMemoryModel
ModInfo   OpEntryPoint*
ModInfo   OpExecutionMode*
INSNS     OpString*
ModDbg    (OpSource|OpSourceContinued|OpSourceExtension)*
ModDbg    OpName*
ModDbg    OpMemberName*
ModDbg    OpModuleProcessed*
ModAnnot  OpDecorate*
ModAnnot  OpDecorateId*
ModAnnot  OpMemberDecorate*
.         OpDecorationGroup*
.         OpGroupDecorate*
.         OpGroupMemberDecorate*
LAYOUT    (OpTypeForwardPointer|IK_T|IK_C|IK_VG)*
(
  OpFunction
  OpFunctionParameter*
  OpFunctionEnd
)*
(
  OpFunction
  OpFunctionParameter*
  (
    OpLabel
    OpVariable*
    <code-non-terminator>*
    <code-terminator>
  )
  (
    OpLabel
    <code-non-terminator>*
    <code-terminator>
  )*
  OpFunctionEnd
)*
#endif
```

## Five

```haskell
-- |
-- >
-- > ppHs $ M.runM (replicateM 2 (do f <- newId; f .= factorial)) 1 1 (empty::MEnv)
-- >
--
factorial :: Build (Ix INSN)
factorial = do
  opMemoryModel AddressingModelPhysical32 MemoryModelOpenCL
  x <- newId
  entry <- newId
  test <- newId
  incr <- newId
  exit <- newId
  boolT <- newId
  i32T <- newId
  one_i32 <- newId
  boolT .= opTypeBool
  i32T .= opTypeInt 32 0
  opLine 0 11112222 22221111
  opLine 0 11112222 22221111
  opLine 0 11112222 22221111
  opLine 0 11112222 22221111
  one_i32 .= opConstant i32T (toNumLit (1::WORD))
  i <- newId
  acc <- newId
  b <- newId
  acc' <- newId
  i' <- newId
  opLine 0 11112222 22221111
  opLine 0 33334444 44443333
  defineFunction_ empty i32T [(x, i32T)] $ do
    entry .: do
      opBranch test
    test .: do
      i .= opPhi i32T [(x, entry), (i', incr)]
      acc .= opPhi i32T [(one_i32, entry), (acc', incr)]
      opLine 0 11112222 22221111
      b .= opULessThanEqual boolT i one_i32
      opNoLine
      opBranchConditional b exit incr []
    opLine 0 33334444 44443333
    incr .: do
      opLine 0 33334444 44443333
      acc' .= opIMul i32T acc i
      i'   .= opISub i32T i one_i32
      opLine 0 33334444 44443333
      opNoLine
      opNoLine
      opBranch test
    exit .: do
      opReturnValue acc
```

```haskell
-- |
-- > /*clang-3.6.1 OpenCL-C*/
-- > kernel void k(unsigned x)
-- > {
-- >   constant const char *fmt = 0;
-- >   switch(x)
-- >   {
-- >     case 0 ... 3:
-- >     case 999999:
-- >       fmt = "AAA\n";
-- >       break;
-- >     case 5 ... 88:
-- >     case 103 ... 204:
-- >       fmt = "BBB\n";
-- >       break;
-- >     case 89 ... 101:
-- >     case 300:
-- >     case 400:
-- >       fmt = "CCC\n";
-- >       break;
-- >     __XXX__:
-- >     case 1000 ... 1004:
-- >     case 1006 ... 1011:
-- >       switch(fmt[0])
-- >       {
-- >         case 'X': fmt = "YYY\n"; break;
-- >         default: fmt = "DDD\n"; break;
-- >       }
-- >       break;
-- >     default:
-- >       fmt = "EEE\n";
-- >       break;
-- >   }
-- >   if(fmt[0] == 'B')
-- >   {
-- >     fmt = "XXXXXXXXXX";
-- >     goto __XXX__;
-- >   }
-- >   printf(fmt);
-- > }
--
module_0 :: Build ()
-- {{{1
module_0 = do
  _L37 <- newId
  let n_L = 18
  [_L38,_L39,_L40,_L41
    ,_L42,_L43,_L44,_L45
    ,_L46,_L47,_L48,_L49
    ,_L50,_L51,_L52,_L53
    ,_L54,_L55
      ] <- replicateM n_L newId
  v11 <- newId
  v14 <- newId
  v17 <- newId
  v20 <- newId
  v23 <- newId
  v26 <- newId
  v32 <- newId
  x58 <- newId
  x59 <- newId
  x60 <- newId
  x61 <- newId
  x62 <- newId
  x63 <- newId
  x64 <- newId
  x66 <- newId
  x69 <- newId
  x70 <- newId
  x72 <- newId
  x74 <- newId
  x75 <- newId
  x76 <- newId
  x77 <- newId
  x78 <- newId
  x79 <- newId
  x80 <- newId
  x81 <- newId
  x82 <- newId
  x83 <- newId
  x85 <- newId
  x86 <- newId
  opCapability CapabilityAddresses
  opCapability CapabilityKernel
  opCapability CapabilityInt8
  eii_1 <- def $ opExtInstImport "OpenCL.std"
  opMemoryModel AddressingModelPhysical32 MemoryModelOpenCL
  f35 <- newId
  fp36 <- newId
  opEntryPoint ExecutionModelKernel f35 "k" []
  opSource SourceLanguageOpenCL_C 200000 Nothing Nothing
  opName v11 ".str"
  opName v14 ".str1"
  opName v17 ".str2"
  opName v20 ".str3"
  opName v23 ".str4"
  opName v26 ".str5"
  opName v32 ".str6"
  opName fp36 "x"
  opName x74 "fmt.0"
  opName x75 "fmt.1"
  opName x79 "fmt.2"
  opName x80 "fmt.3"
  dg87 <- def $ opDecorationGroup
  dg88 <- def $ opDecorationGroup
  opGroupDecorate dg87 [v11,v14,v17,v20,v23,v26,v32]
  opGroupDecorate dg88 [v11,v14,v17,v20,v23,v26,v32]
  opDecorate dg87 DecorationConstant
  opDecorate dg88 (DecorationAlignment 1)
  t2 <- def $ opTypeInt 8 0
  t6 <- def $ opTypeInt 32 0
  t33 <- def $ opTypeVoid
  t68 <- def $ opTypeBool
  c7 <- def $ opConstant t6 (toNumLit (5::WORD))
  c28 <- def $ opConstant t6 (toNumLit (11::WORD))
  t8 <- def $ opTypeArray t2 c7
  t29 <- def $ opTypeArray t2 c28
  t57 <- def $ opTypePointer StorageClassUniformConstant t2
  t10 <- def $ opTypePointer StorageClassUniformConstant t8
  t31 <- def $ opTypePointer StorageClassUniformConstant t29
  t34 <- def $ opTypeFunction t33 [t6]
  x73 <- def $ opConstantNull t57
  c3 <- def $ opConstant t2 (toNumLit (65::WORD))
  c4 <- def $ opConstant t2 (toNumLit (10::WORD))
  c5 <- def $ opConstant t2 (toNumLit (0::WORD))
  c12 <- def $ opConstant t2 (toNumLit (66::WORD))
  c15 <- def $ opConstant t2 (toNumLit (67::WORD))
  c18 <- def $ opConstant t2 (toNumLit (89::WORD))
  c21 <- def $ opConstant t2 (toNumLit (68::WORD))
  c24 <- def $ opConstant t2 (toNumLit (69::WORD))
  c27 <- def $ opConstant t2 (toNumLit (88::WORD))
  c56 <- def $ opConstant t6 (toNumLit (0::WORD))
  c65 <- def $ opConstant t6 (toNumLit (103::WORD))
  c67 <- def $ opConstant t6 (toNumLit (101::WORD))
  c71 <- def $ opConstant t6 (toNumLit (83::WORD))
  c84 <- def $ opConstant t6 (toNumLit (66::WORD))
  c9 <- def $ opConstantComposite t8 [c3,c3,c3,c4,c5]
  c13 <- def $ opConstantComposite t8 [c12,c12,c12,c4,c5]
  c16 <- def $ opConstantComposite t8 [c15,c15,c15,c4,c5]
  c19 <- def $ opConstantComposite t8 [c18,c18,c18,c4,c5]
  c22 <- def $ opConstantComposite t8 [c21,c21,c21,c4,c5]
  c25 <- def $ opConstantComposite t8 [c24,c24,c24,c4,c5]
  c30 <- def $ opConstantComposite t29 [c27,c27,c27,c27,c27,c27,c27,c27,c27,c27,c5]
  v11 .= defineVariable t10 StorageClassUniformConstant c9
  v14 .= defineVariable t10 StorageClassUniformConstant c13
  v17 .= defineVariable t10 StorageClassUniformConstant c16
  v20 .= defineVariable t10 StorageClassUniformConstant c19
  v23 .= defineVariable t10 StorageClassUniformConstant c22
  v26 .= defineVariable t10 StorageClassUniformConstant c25
  v32 .= defineVariable t31 StorageClassUniformConstant c30
  f35 .= defineFunction_ empty t33 [(fp36, t6)] $ do
    _L37 .: do
      x58 .= opInBoundsPtrAccessChain t57 v32 c56 [c56]
      x59 .= opInBoundsPtrAccessChain t57 v23 c56 [c56]
      x60 .= opInBoundsPtrAccessChain t57 v20 c56 [c56]
      x61 .= opInBoundsPtrAccessChain t57 v14 c56 [c56]
      x62 .= opInBoundsPtrAccessChain t57 v26 c56 [c56]
      x63 .= opInBoundsPtrAccessChain t57 v17 c56 [c56]
      x64 .= opInBoundsPtrAccessChain t57 v11 c56 [c56]
      opSwitch fp36 _L43
        [(toNumLit (999999::WORD),_L39)
        ,(toNumLit (0::WORD),_L38)
        ,(toNumLit (1::WORD),_L38)
        ,(toNumLit (2::WORD),_L38)
        ,(toNumLit (3::WORD),_L38)
        ,(toNumLit (300::WORD),_L45)
        ,(toNumLit (400::WORD),_L45)
        ,(toNumLit (89::WORD),_L44)
        ,(toNumLit (90::WORD),_L44)
        ,(toNumLit (91::WORD),_L44)
        ,(toNumLit (92::WORD),_L44)
        ,(toNumLit (93::WORD),_L44)
        ,(toNumLit (94::WORD),_L44)
        ,(toNumLit (95::WORD),_L44)
        ,(toNumLit (96::WORD),_L44)
        ,(toNumLit (97::WORD),_L44)
        ,(toNumLit (98::WORD),_L44)
        ,(toNumLit (99::WORD),_L44)
        ,(toNumLit (100::WORD),_L44)
        ,(toNumLit (101::WORD),_L44)
        ,(toNumLit (1006::WORD),_L48)
        ,(toNumLit (1007::WORD),_L48)
        ,(toNumLit (1008::WORD),_L48)
        ,(toNumLit (1009::WORD),_L48)
        ,(toNumLit (1010::WORD),_L48)
        ,(toNumLit (1011::WORD),_L48)
        ,(toNumLit (1000::WORD),_L47)
        ,(toNumLit (1001::WORD),_L47)
        ,(toNumLit (1002::WORD),_L47)
        ,(toNumLit (1003::WORD),_L47)
        ,(toNumLit (1004::WORD),_L47)]
    _L38 .: do
      opBranch _L39
    _L39 .: do
      opBranch _L53
    _L40 .: do
      opBranch _L41
    _L41 .: do
      opBranch _L53
    _L42 .: do
      x66 .= opISub t6 fp36 c65
      x69 .= opULessThanEqual t68 x66 c67
      opBranchConditional x69 _L41 _L52 []
    _L43 .: do
      x70 .= opISub t6 fp36 c7
      x72 .= opULessThanEqual t68 x70 c71
      opBranchConditional x72 _L40 _L42 []
    _L44 .: do
      opBranch _L45
    _L45 .: do
      opBranch _L53
    _L46 .: do
      opBranch _L47
    _L47 .: do
      x74 .= opPhi t57
        [(x58,_L46)
        ,(x73,_L37)
        ,(x73,_L37)
        ,(x73,_L37)
        ,(x73,_L37)
        ,(x73,_L37)]
      opBranch _L48
    _L48 .: do
      x75 .= opPhi t57
        [(x74,_L47)
        ,(x73,_L37)
        ,(x73,_L37)
        ,(x73,_L37)
        ,(x73,_L37)
        ,(x73,_L37)
        ,(x73,_L37)]
      x76 .= opInBoundsPtrAccessChain t57 x75 c56 []
      x77 .= opLoad t2 x76 (Just empty{memoryAccessAligned=Just 1})
      x78 .= opSConvert t6 x77
      opSwitch x78 _L50 [(toNumLit (88::WORD),_L49)]
    _L49 .: do
      opBranch _L51
    _L50 .: do
      opBranch _L51
    _L51 .: do
      x79 .= opPhi t57
        [(x59,_L50)
        ,(x60,_L49)]
      opBranch _L53
    _L52 .: do
      opBranch _L53
    _L53 .: do
      x80 .= opPhi t57
        [(x61,_L41)
        ,(x79,_L51)
        ,(x62,_L52)
        ,(x63,_L45)
        ,(x64,_L39)]
      x81 .= opInBoundsPtrAccessChain t57 x80 c56 []
      x82 .= opLoad t2 x81 (Just empty{memoryAccessAligned=Just 1})
      x83 .= opSConvert t6 x82
      x85 .= opIEqual t68 x83 c84
      opBranchConditional x85 _L54 _L55 []
    _L54 .: do
      opBranch _L46
    _L55 .: do
      x86 .= opExtInst t6 eii_1 6666666{-printf-} [x80]
      opReturn
  return ()
-- }}}1
```

## Six

```haskell
#if 0
--
-- GOOOOOOOOOOOOOOOOOOOD
-- GOOOOOOOOOOOOOOOOOOOD
-- GOOOOOOOOOOOOOOOOOOOD
-- GOOOOOOOOOOOOOOOOOOOD
-- GOOOOOOOOOOOOOOOOOOOD
-- GOOOOOOOOOOOOOOOOOOOD
-- GOOOOOOOOOOOOOOOOOOOD
-- GOOOOOOOOOOOOOOOOOOOD
-- GOOOOOOOOOOOOOOOOOOOD
--
(Right m_,Right m) <- testWithSpirvFile "/home/m/int64_case_labels.cl.spv"
ppHs m_
ppHs m
ppHs . fmap toValEnum $ m
ppHs . fmap (\x->let (a,b) = splitRHS x in ((a,idDefsTypesOfL x),b::OpRHS))  $ m
#endif

#if 0
ghci> let (cxt,(fm,m)) = test_3
ghci> let xs  = Ix.keysSet  . udDefX . moduleUD $ m
ghci> let ys = Ix.keysSet  . udUseX . moduleUD $ m
ghci> ys\\xs
mempty
ghci> ppHs . (Ix.! 44)  . udUseX . moduleUD $ m
PortMap{portMapIP = [(19, [(0, ())]),
                     (121, [(1, ())]),
                     (129, [(2, ())])],
        portMapPI = [(0, [(19, ())]), (1, [(121, ())]), (2, [(129, ())])]}
ghci> ppHs . (Ix.! 19)  . udUseI . moduleUD $ m
PortMap{portMapIP = [(44, [(0, ())])],
        portMapPI = [(0, [(44, ())])]}
ghci> ppHs . (Ix.! 121)  . udUseI . moduleUD $ m
PortMap{portMapIP = [(44, [(1, ())]),
                     (63, [(0, ())]),
                     (77, [(2, ())])],
        portMapPI = [(0, [(63, ())]), (1, [(44, ())]), (2, [(77, ())])]}
ghci> ppHs . (Ix.! 129)  . udUseI . moduleUD $ m
PortMap{portMapIP = [(44, [(2, ())]),
                     (50, [(1, ())]),
                     (56, [(0, ())])],
        portMapPI = [(0, [(56, ())]), (1, [(50, ())]), (2, [(44, ())])]}
#endif

#if 0
ghci> mapM_ print . OM.toList . fmap Ix.keys . insnInfoKindInv  . moduleInsnInfo $ m
(IK_CODE,[71,72,73,74,75,76,77,78,80,82,84,86,88,89,90,92,93,94,96,98,100,102,103,105,106,107,108,109,111,113,115,116,118,120,121,122,123,124,125,127,129,130])
(IK_B,[70,79,81,83,85,87,91,95,97,99,101,104,110,112,114,117,119,126,128])
(IK_FP,[69])
(IK_F,[68])
(IK_VG,[60,61,62,63,64,65,66])
(IK_C,[30,31,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59])
(IK_T,[26,27,28,29,32,33,34,35,36,37,67])
(IK_DG,[20,21])
(IK_EII,[4])
(IK_AUG_MOD,[1,2,3,5,7])
(IK_AUG_ID,[6,8,9,10,11,12,13,14,15,16,17,18,19,22,23,24,25])
(IK_FUN_END,[131])
#endif
```

## Seven

...

