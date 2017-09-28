# github-md-testing

.. code:: haskell
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

