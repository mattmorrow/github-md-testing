-----------------------------------------------------------------------------

signature EG_ASN1 = sig
-- {{{
  data PresenceConstraint = Present | Absent | Optional
  data Constraint = Constraint ElementSets (Maybe ExceptionIdentification)
  data ExceptionIdentification
    = ExceptionNumber Integer
    | ExceptionValue DefinedValue
    | ExceptionTypeAndValue Type Value
  data NamedConstraint = NamedConstraint Identifier ComponentConstraint
  data ComponentConstraint = ComponentConstraint (Maybe Constraint) (Maybe PresenceConstraint)
  data ElementSets
    = ClosedSet Bool ElementSet
    | ExtendableSet ElementSet
    | SetRange ElementSet ElementSet
  data ElementSet = AllExcept Exclusions | Union [[Intersection]] | Singleton Elements
  data Intersection = Intersection Elements (Maybe Exclusions)
  data Elements
    = Subset ElementSet
    | Subtype SubtypeElements
    | ObjSet ObjectSetElements
  data ValueRangeEndValue
    = MinValue
    | MaxValue
    | Value Value
  data ValueRangeEndpoint
    = Closed ValueRangeEndValue
    | Open ValueRangeEndValue
  data SubtypeElements
    = SingleValue Value
    | ContainedSubtype Type
    | ValueRange ValueRangeEndpoint ValueRangeEndpoint
    | PermittedAlphabet Constraint
    | SizeConstraint Constraint
    | SingleTypeConstraint Constraint
    | MultipleTypeConstaints TypeConstraints
    | PatternConstraint CString
  data TagType = Explicit | Implicit
  data Tag = Tag (Maybe Class) ClassNumber
  data ClassNumber = ClassNumber Integer | ClassNumberAsDefinedValue DefinedValue
  data Class = Universal | Application | Private
  data ComponentType
    = NamedTypeComponent
        {element_type :: NamedType
        ,element_presence :: Maybe ValueOptionality}
    | ComponentsOf Type
  data ValueOptionality
    = OptionalValue
    | DefaultValue Value
  data Type = Type
    {type_id :: BuiltinType
    ,subtype :: Maybe Constraint}
-- }}}

-----------------------------------------------------------------------------

#if 0
(op,in_t,out_t)
OpBitcastTAG         a(n)      b(n)
OpUConvertTAG        i(m)      i(n)
OpSConvertTAG        i(m)      i(n)
OpFConvertTAG        f(m)      f(n)
OpQuantizeToF16TAG   f(m<16)   f(16)
OpSatConvertSToUTAG  i(m>n)    b(n)
OpSatConvertUToSTAG  i(m>n)    i(n)
OpConvertFToUTAG     f(m)      i(n)
OpConvertFToSTAG     f(m)      i(n)
OpConvertSToFTAG     i(m)      f(n)
OpConvertUToFTAG     i(m)      f(n)
OpConvertPtrToUTAG   p(m,_,_)  i(n)
OpConvertUToPtrTAG   i(m)      p(n,_,_)
OpPtrCastToGenericTAG
  p(m,WorkGroup|CrossWorkGroup|Function,a(n))
  p(m,Generic,a(n))
OpGenericCastToPtrTAG
  p(m,Generic,a(n))
  p(m,WorkGroup|CrossWorkGroup|Function,a(n))
OpGenericCastToPtrExplicitTAG<s>
  p(m,Generic,a(n))
  p(m,s|WorkGroup|CrossWorkGroup|Function,a(n))
#endif

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

-----------------------------------------------------------------------------

#if 0
class XXXXXXX a where
class (Monad m) => CompilerM m where
  -- .........
  -- .........
  -- .........
#if 0
insertEdge    :: Ix EDGE -> Edge -> M ()
deleteEdge    :: Ix EDGE -> M ()
moveEdge      :: Ix EDGE -> Edge -> M ()
splitEdge     ::
collapseEdge  ::
#endif
class (CompilerM m) => GVN m ver conf env out | ver -> conf env out where
  gvnM :: ver -> Ix FUNCTION -> m out
  gvnOpenM  :: ver -> Ix FUNCTION -> m env
  gvnStepM  :: ver -> env -> m (Maybe env)
  gvnCloseM :: ver -> env -> m out
  gvnM ver fun = gvnCloseM ver =<< loop =<< gvnOpenM ver fun
    where loop env = do
            o <- gvnStepM ver env
            case o of
              Just env-> loop env
              Nothing-> return env
data GvnOut   = GvnOut  {}  __D
data GvnEnv   = GvnEnv  {}  __D
data GvnConf  = GvnConf {}  __D
data Val
  = PortV         !Pos              !(Ix VAL)
  | BinAssocV     !Op   !(Ix INST)  [Ix VAL]
  | BinAssocCommV !Op   !(Ix INST)  [Ix VAL]
  -- ...
  -- ...
  -- ...
  | LoadV         !Op   !(Ix INST)  [Ix VAL]
  | StoreV        !Op   !(Ix INST)  [Ix VAL]
  | CallV         !Op   !(Ix INST)  [Ix VAL]
  | PhiV                            [Ix VAL]
  | SgmV                            !(Ix VAL)
  | OtherV              !(Ix INST)  [Ix VAL]
  __D
#endif

-----------------------------------------------------------------------------

#if 0
-- {{{1
-- {{{
signature CFG_attempt_3 = sig
{
  data Function = Function
    {functionType :: !(Ix INSN)
    ,functionCtl :: FunctionControl
    ,functionParams :: [(Ix INSN, Ix INSN, Ix INSN)]
    ,functionVars :: [Ix INSN]
    ,functionEntry  :: !(Ix INSN)
    ,functionBlocks :: IxMap INSN ()}
  data Block = Block
    {blockIn :: BlockIn
    ,blockOut :: BlockOut
    ,blockMids :: [Ix INSN]}
  data BlockIn = BlockIn
    {
    --blockInSrcs :: [Ix ID]
    blockInPhiI :: [Ix INSN]
    --,blockInPhiX :: [Ix ID]
    --,blockInPhis :: IxMap ID [Ix ID]
    }
  data BlockOut = BlockOut
    {blockOutOp :: !Int
    ,blockOutI :: !(Ix INSN)
    ,blockOutX :: Maybe (Ix INSN)
    ,blockOutTgts :: [Ix INSN]
    ,blockOutVals :: [NumLit]
    ,blockOutWeights :: [WORD]}
}
-- }}}

-- {{{
signature CFG_attempt_2 = sig
{
  data CFG = CFG
    {
     xcfgInputs  :: PosMap (Ix ID)
    ,xcfgOutputs :: PosMap (Ix ID)
    ,xcfgEntry   :: !(Ix BLOCK)
    ,xcfgExit    :: !(Ix BLOCK)
    ,xcfgBlocks  :: IxMap BLOCK Block
    }
  data Block = Block
    {
     blockNIn   :: !Size
    ,blockNOut  :: !Size
    ,blockIn    :: BlockIn
    ,blockOut   :: BlockOut
    ,blockPhis  :: PhiMat
    ,blockSgms  :: SgmMat
    ,blockLast  :: BlockLast
    ,blockInsns :: [Ix INSN]
    }
  type BlockIn = PosMap (Port BLOCK)
  type BlockOut = PosMap (Port BLOCK)
  data BlockLast = BlockLast
    {blockLastVar :: !(Ix ID)
    ,blockLastPos :: PosMap INT
    ,blockLastVal :: INTMap Pos}
  type INT = Int
  type INTMap a = IntMap a
  type INTSet = INTMap ()
}
-- }}}

-- {{{
signature CFG_attempt_1 = sig
{
  data CFG = CFG
    {cfgIn :: !VEC(Ix VAR)
    ,cfgOut :: !VEC(Ix VAR)
    ,cfgEntry :: !(Ix BLOCK)
    ,cfgExits :: !(IxSet BLOCK)
    ,cfgBlocks :: !(IxSet BLOCK)}
  data CFGInfo = CFGInfo
    {
    cfgV2T             :: Trans VAR TYPE
    ,cfgI2B             :: Trans INSN BLOCK
    ,cfgVDef            :: Trans VAR INSN
    ,cfgVUse            :: Cov VAR INSN
    ,cfgVDed            :: Cov VAR INSN
    --
    ,cfgInfoEntry             :: !(Ix BLOCK)
    ,cfgInfoExit              :: !(Ix BLOCK)
    ,cfgInfoEdge              :: IxMap EDGE Edge
    ,cfgInfoEdges             :: IxMap EDGES Edges
    ,cfgInfoBlocks            :: IxMap BLOCK BlockInfo
    ,cfgInfoSCCsB             :: SCCs BLOCK
    ,cfgInfoUnreachableRoots  :: IxSet_ (SCC BLOCK)
    ,cfgInfoUnescapableLeaves :: IxSet_ (SCC BLOCK)
    ,cfgInfoAugmented         :: Graph BLOCK
    ,cfgInfoFakeEntryEdges    :: IxSet_ BLOCK
    ,cfgInfoFakeExitEdges     :: IxSet_ BLOCK
    ,cfgInfoDom               :: CFGDomInfo
    ,cfgInfoPDom              :: CFGDomInfo
    }
  data CFGDomInfo = CFGDomInfo
    {cfgDomInfoDomTree :: DomTree BLOCK
    ,cfgDomInfoDomFront :: DomFront BLOCK
    ,cfgDomInfoDomFrontPlus :: DomFrontPlus BLOCK}
  data Edges = Edges
    {edgesSrc :: !(Ix BLOCK)
    ,edgesTgt :: !(Ix BLOCK)
    ,edgesSize :: !Size
    ,edgesPos :: PosMap Pos
    ,edgesEdge :: IxSet_ EDGE}
  data Edge = Edge
    {edgeSrc :: !(Port BLOCK)
    ,edgeTgt :: !(Port BLOCK)
    ,edgeEdges :: !(Ix EDGES)}
  data Last = Last
    {lastCases :: IntMap (Ix EDGE)
    ,lastDefault :: !(Ix EDGE) -- ^ 0 ==> cases are (possibly dynamically) exhaustive
    }
  data BlockInfo = BlockInfo
    {blockInfoIn :: BlockInOut
    ,blockInfoOut :: BlockInOut
    ,blockInfoLast :: Last}
  data BlockInOut = BlockInOut
    {blockInOutES :: IxSet_ EDGES
    ,blockInOutB2ES :: IxMap BLOCK (Ix EDGES)
    ,blockInOutP2ES :: PosMap (Ix EDGE)}
  -------------------------------
  data BlockIn = BlockIn {}
  data BlockOut = BlockOut {}
  data Block = Block
    {blockIn :: BlockIn
    ,blockOut :: BlockOut
    ,blockInsns :: VEC(Ix INSN)
    ,blockAttrs :: Attrs}
}
-- }}}
-- }}}1
#endif

-----------------------------------------------------------------------------
