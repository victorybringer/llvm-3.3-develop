{-# LANGUAGE BangPatterns,ViewPatterns,DeriveGeneric,RankNTypes,
             TemplateHaskell,DeriveAnyClass,FlexibleInstances,CPP #-}
 

module LLVM.Slicing.Static.SDG.SDGSlicer2 (
 -- * Types
 SliceSummary(..),
 -- * Slice Computing
 computeSlice,
 genSliceTable, genSliceTable2,
 getBwdSli, getFwdSli,
 genSDG
 )
 where

import Control.Arrow 
import GHC.Generics ( Generic )
import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Lens', makeLenses, (.~), (%~), (^.) )

import Data.Map ( Map )
import qualified Data.Map as M
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Set ( Set )
import qualified Data.Set as S
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM ( fromList )
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.Tuple ( swap )
import Data.Maybe 
--import Data.Monoid 
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.List  
import Data.Char ( isLetter )
--import qualified Data.Text as T ( unpack )
import Text.Printf ( printf )

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.CallGraphSCCTraversal
import LLVM.Analysis.Dataflow
import LLVM.Analysis.CallGraph
import LLVM.Analysis.PointsTo
import LLVM.Analysis.PointsTo.TrivialFunction
--import LLVM.Analysis.PointsTo.Andersen

import LLVM.Slicing.Util.Utils
import LLVM.Slicing.Data.SliceType
import LLVM.Slicing.Static.SDG.SDGType        
import LLVM.Slicing.Static.SDG.SDGADT

import qualified Data.Graph.Inductive as G


------
data SliceSummary =
  SliceSummary {  _sdgSummary :: !SDGGraphType     
                , _funModSet :: IntMap (HashSet Value)
                , _funSummEdges :: IntMap (Set ((Int,Int),Bool))  
                , _traceSize :: Int
                }
  deriving (Generic)

$(makeLenses ''SliceSummary)

instance Eq SliceSummary where
  (SliceSummary g1 m1 s1 tr1) == (SliceSummary g2 m2 s2 tr2) = 
         (gEqual g1 g2) && m1 == m2 && s1 == s2       -- && tr1 == tr2

--instance Monoid SliceSummary where
--  mempty = SliceSummary G.empty mempty mempty 0
--  mappend (SliceSummary sg1 fms1 se1 tr1) (SliceSummary sg2 fms2 se2 tr2) =
--    SliceSummary (gUnion sg1 sg2)(IM.unionWith HS.union fms1 fms2)
--                 (IM.unionWith S.union se1 se2)(tr1 + tr2)

instance Semigroup SliceSummary where
  (SliceSummary sg1 fms1 se1 tr1) <> (SliceSummary sg2 fms2 se2 tr2) =
    SliceSummary (gUnion sg1 sg2)(IM.unionWith HS.union fms1 fms2)
                 (IM.unionWith S.union se1 se2)(tr1 + tr2)

instance Monoid SliceSummary where
  mempty = SliceSummary G.empty mempty mempty 0
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif

instance {-# OVERLAPS #-} NFData SliceSummary where
  rnf (SliceSummary !g !_ !_ _) = ()

--
data SliceAnalysis = SliceAnalysis { _sliceSumm :: SliceSummary }
  deriving (Eq, Generic)

$(makeLenses ''SliceAnalysis)

instance {-# OVERLAPS #-} NFData SliceAnalysis where
  rnf !_ = ()

--instance Monoid SliceAnalysis where
--  mempty = SliceAnalysis { _sliceSumm = mempty }
--  mappend a1 a2 =
--    SliceAnalysis { _sliceSumm = _sliceSumm a1 `mappend` _sliceSumm a2 }

instance Semigroup SliceAnalysis where
  a1 <> a2 =
    SliceAnalysis { _sliceSumm = _sliceSumm a1 <> _sliceSumm a2 }

instance Monoid SliceAnalysis where
  mempty = SliceAnalysis { _sliceSumm = mempty }
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif

---------------
----
top :: SliceInfo
top = SInfo mempty mempty mempty mempty   


meetSliceInfo :: SliceInfo -> SliceInfo -> SliceInfo
meetSliceInfo (SInfo ns1 es1 in1 out1) (SInfo ns2 es2 in2 out2) = 
  SInfo (IM.union ns1 ns2)(M.union es1 es2)(IM.unionWith S.union in1 in2)(IM.unionWith S.union out1 out2)

sliceTransfer :: SliceInfo -> Instruction -> Analysis SliceInfo
sliceTransfer si i =  do  
   addTrace i
   case i of
      CallInst { callFunction = fv, callArguments = (map fst -> cargs) } -> do
           si' <- addNodesEdgesAtCall fv cargs si i
           computeInOutWith (Just (fv,cargs)) si' i
      InvokeInst { invokeFunction = fv, invokeArguments = (map fst -> cargs) } -> do
           si' <- addNodesEdgesAtCall fv cargs si i
           computeInOutWith (Just (fv,cargs)) si' i
      _ -> computeInOutWith Nothing si i                                  
{-# INLINE sliceTransfer #-}   


identifySlice ::    
       (FuncLike funcLike, HasFunction funcLike, HasCFG funcLike)
        => Module -> Lens' compositeSummary SliceSummary
        -> ComposableAnalysis compositeSummary funcLike
identifySlice m lns =
  composableAnalysisM runner (sliceAnalysis m) lns
  where
    runner a = runAnalysis a constData cache
    constData = SEnv undefined mempty mempty mempty 
    cache = SState 0 mempty


sliceAnalysis :: (FuncLike funcLike, HasCFG funcLike,HasFunction funcLike)
               => Module -> funcLike -> SliceSummary -> Analysis SliceSummary
sliceAnalysis m funcLike s@(SliceSummary sg fms se _) = do 
  let envMod e = e { fCFG = controlFlowGraph f    
                   , fModSet = fms'
                   , summEdges = se
                   , paraValMap = fParaMap 
--                   , instCtrMap = genCtrMap f
                   } 
--      sg0 = insEdges2 fLEdges $ G.insNodes fLNodes sg
      fact0 =  (edgeMap %~ M.union fEdges). (nodeMap %~ IM.union fLNodes) $ 
              (outMap %~ IM.insertWith S.union fEntryId fEntryOut) top         
      analysis = fwdDataflowAnalysis top meetSliceInfo sliceTransfer  -- fwdDataflowAnalysis
--  instCtrM <- analysisEnvironment instCtrMap
  localInfo <- analysisLocal envMod (dataflow funcLike analysis fact0)  -- dataflow
  tr <- getTrace 
  let trStr = fName ++ fStrLn ++ "\'s #Insts_traced = " ++ show tr -- (length tr)
      fStrLn = case getFunctionLine f of
               Just (src, ln) -> "(Defined at " ++ src ++ ":" ++ show ln ++ ")"
               Nothing        -> ""
      si@(SInfo nsM esM _ _) = dataflowResult localInfo 
--      fInstCtrMap = IM.filterWithKey (\k _ -> elem k $ map valueUniqueId fInsts) instCtrM  
--      cdEdges = M.fromList [((b,i),ControlDepEdge) |
--                            (i, bs) <- IM.toList fInstCtrMap, b <- IS.toList bs]
      fOutSet = S.toList. S.unions. map (lkpOut si) $ functionExitInstructions f
      outEdges = M.fromList $ mapMaybe getOutEdge fOutSet   --  filter (\(a,b)-> a /= b)
         where  getOutEdge (i,v) = do { vn <- toParaOutNode v; return ((vn,i),DataDepEdge)}                    
      toLEdges em = [(a,b,t) | ((a,b),t) <- M.toList em]
      sg' = -- timeIt (fName ++ "_SDG-Builder").
         insEdges2 (toLEdges $ M.unions [outEdges,esM])   -- cdEdges
           $ G.insNodes (IM.toList nsM) sg
--      sgNew = insEdges2 (cdEdges ++ outEdges) sg'    -- G.insEdges
      fSummEdge = -- timeIt2 (fName ++ "_SDG-SummEdge"). 
               S.fromList $ concatMap getSummEdge argOutNodes 
         where  toAlias ai = maybeToList $ do 
                          nlab <- lookup ai fParaNodes
                          alias <- IM.lookup (getUID nlab) fAliasMap 
                          return $ alias - div maxNumID 2
                argInNodes = filter (> 0) $ map fst fParaNodes
                argOutNodes = map negate argInNodes
                sdg = G.elfilter (flip notElem [ParaInEdge,ParaOutEdge,CallEdge]) sg'
                getSummEdge ai = [((ai,bi),isDelOut ai) | bi <- getInNodes ai]
                   where reachNodes n = G.dfs (n : (toAlias n)) sdg
                         getInNodes = intersect argInNodes. reachNodes 
                         isDelOut ai = null $ (nub $ G.dfs [ai] sdg) \\ [fID,-ai,ai]
      sgNew = G.delNodes delOutNodes sg'
         where delOutNodes = S.toList. S.map (fst . fst) $ S.filter snd fSummEdge
  return $ (funModSet .~ fms') 
         $ (funSummEdges %~ IM.insert fID fSummEdge) 
         $ (traceSize %~ (+) tr)
         $! (sdgSummary %~ gUnion sgNew) s     --  `debug` trStr
  where
    f = getFunction funcLike  
    fID = functionUniqueId f              
    fName = identifierAsString (functionName f)
    fAliasSet = getFunAlias' f
    fAliasMap = IM.fromList. mapBoth valueUniqueId valueUniqueId $ fAliasSet
    fAliasMap2 = HM.fromList. map swap $ fAliasSet
    fms' = addFunMod fAliasMap2 fms f
    fEntryId = instructionUniqueId $ functionEntryInstruction f
--    fRefIds = HS.toList. HS.map valueUniqueId $ functionRefs fAliasMap2 f
    fEntryOut = S.fromList [ (n,getUID lab) | (n,lab) <- fArgInNodes ]   
       where fArgInNodes = filter ((> 0). fst) fParaNodes
    fInsts = functionInstructions f
    fParaNodes = concatMap toVarNode allParaVals
       where toVarNode (v,n) 
               | n == -2    =  [(vi - div maxNumID 2,FinalUseNode vi)]
               | n == -1    =  [(getNewId fID vi,GlobalVariableInNode fID vi),
                                (- getNewId fID vi,GlobalVariableOutNode fID vi)]
               | otherwise  =  [(vi,FormalInNode vi),(-vi,FormalOutNode vi)]  
               where   vi = valueUniqueId v
    fLNodes = IM.fromList $ (fID, FunctionEntryNode fID) : fParaNodes 
            ++ map (valueUniqueId &&& (InstructionNode. valueUniqueId)) fInsts
    fEdges = M.fromList $ map (\i ->((i, fID), ControlDepEdge))
                              (fTopInsts ++ map fst fParaNodes) 
    fTopInsts = map instructionUniqueId . concatMap basicBlockInstructions $ 
                      (functionBody f) \\ (concatMap getInnerBBs fInsts)
    getInnerBBs i =  case i of
         BranchInst {branchTrueTarget = tb, branchFalseTarget = fb} -> [tb,fb]
         SwitchInst {switchDefaultTarget = db, switchCases = cases} -> db : map snd cases
         IndirectBranchInst {indirectBranchTargets = bs} -> bs
         _ -> []
    toParaOutNode vi = case IM.lookup vi fParaMap of 
               Just (_, -2) ->  Just (vi - div maxNumID 2)
               Just (_, -1) ->  Just (-(getNewId fID vi))
               Just _   ->  Just (-vi)
               Nothing  -> Nothing
    ---
    fParaMap = IM.fromList $ zip allParaIds allParaVals
    allParaVals = globalVals ++ fAllocVals ++ fArgVals    
    allParaIds = map (valueUniqueId . fst) allParaVals
    globalVals = zip (filter ((\(k:_)->(isLetter k || k == '_')). drop 1. toVarName').
                       map toValue $ moduleGlobalVariables m) (repeat (-1))
--    glbIds = map (valueUniqueId . fst) globalVals
    fAllocVals = zip (map toValue $ funcAllocInsts f) (repeat (-2)) 
    fArgVals = zip (map toValue $ functionParameters f) [0..]
--    fArgIds = map (valueUniqueId . fst) fArgVals


type IsParallel = Bool
computeSlice :: IsParallel -> Module -> SliceSummary
computeSlice isPar m = _sliceSumm res  -- `showGraph` (cg,mName)
  where
    cg = callGraph m pta [] 
    pta = runPointsToAnalysis m
--    mName = T.unpack $ moduleIdentifier m
    analyses :: [ComposableAnalysis SliceAnalysis Function]
    analyses = [ identifySlice m sliceSumm ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = cgTraversal cg analysisFunc mempty  
    cgTraversal = if isPar then parallelCallGraphSCCTraversal else callGraphSCCTraversal    

---------------
genSliceTable2 :: IsParallel -> Module -> (Map String [String],Map String [String])
genSliceTable2 isPar m = (toSrcLns bwdST, toSrcLns fwdST)
  where (bwdST,fwdST) = genSliceTable isPar m
        valMap = genValueMap m
        toSrcLns = M.map (toSrcLnStr valMap)

genSliceTable :: IsParallel -> Module -> (SliceTable,SliceTable)    
genSliceTable isPar m = (slices getBwdSli, slices getFwdSli)
  where sdg = genSDG isPar m
        fs = moduleDefinedFunctions m     
        allVals =  map toValue (concatMap functionParameters fs)
                ++ map toValue (moduleGlobalVariables m) 
                ++ map toValue (concatMap funcAllocInsts fs)
        allVals' = filter ((\(k:_)->(isLetter k || k == '_')). drop 1. toVarName') allVals
        vars = map (drop 1 . toVarName') $ allVals'
        slices getSliF = M.fromList. zip vars $ map (getSliF m sdg. (:[])) allVals' 

genSDG :: IsParallel -> Module -> SDGGraphType
genSDG isPar m = sdg  `debug` (trStr ++ sdgSize)
  where aliasMap = genAliasMap2 m
        summary = computeSlice isPar m
        sdg = addAliasEdge aliasMap $! _sdgSummary summary          
        trStr = "\n\tIts trace Info: #Insts_SDG = " ++ show (_traceSize summary)    
        sdgSize = printf "\n\tIts SDG(#Nodes,#Edges) = (%s,%s)" 
                  (show $ G.noNodes sdg)(show . length $ G.labEdges sdg) 
             

getBwdSli :: IsValue a => Module -> SDGGraphType -> [a] -> ValueIds
getBwdSli m sdg vs = IS.fromList $ slice1 ++ slice2  
  where slice1 = G.dfs nodes $ G.elfilter (/= ParaOutEdge) sdg   
        slice2 = G.dfs slice1 $ G.elfilter (flip notElem [ParaInEdge,CallEdge]) sdg 
--        sdg = _sdgSummary $ computeSlice m
        nodes = map toFinalUse vs
        topFunId  = maybe topFunNode functionUniqueId (findMain m) 
           where topFunNode = head . sortBy (\a b -> compare (G.indeg sdg a) (G.indeg sdg b)).
                                  mapMaybe getFuncId. map snd $ G.labNodes sdg
                 getFuncId (FunctionEntryNode fi) = Just fi
                 getFuncId _  = Nothing
        toFinalUse v = case valueContent' v of 
            InstructionC i@AllocaInst {} -> valueUniqueId i - div maxNumID 2
            GlobalVariableC g -> let gNode = - getNewId topFunId (valueUniqueId g) in
                     if G.gelem gNode sdg then gNode else -gNode   
            ArgumentC a -> let aNode = - valueUniqueId a in
                     if G.gelem aNode sdg then aNode else -aNode   
            _ -> valueUniqueId v  

getFwdSli :: IsValue a => Module -> SDGGraphType -> [a] -> ValueIds              
getFwdSli m sdg vs = IS.fromList $ slice1 ++ slice2  
  where slice1 = G.dfs nodes $ G.elfilter (flip notElem [ParaInEdge,CallEdge]) sdg'   
        slice2 = G.dfs slice1 $ G.elfilter (/= ParaOutEdge) sdg'    
        sdg' = G.grev sdg
        nodes = concatMap toFinalDef vs
        topFunId  = maybe topFunNode functionUniqueId (findMain m) 
           where topFunNode = head . sortBy (\a b -> compare (G.indeg sdg a) (G.indeg sdg b)).
                                  mapMaybe getFuncId. map snd $ G.labNodes sdg
                 getFuncId (FunctionEntryNode fi) = Just fi
                 getFuncId _  = Nothing
        toFinalDef v = case valueContent' v of 
            InstructionC i@AllocaInst {} -> finalDefNodes
              where finalUseNode = valueUniqueId i - div maxNumID 2
                    finalUseDeps = G.pre (G.elfilter (==DataDepEdge) sdg') finalUseNode
                    finalDefNodes = if null finalUseDeps then [valueUniqueId v]
                                    else finalUseDeps     -- filter isDefNode finalUseDeps
                    isDefNode n = case G.lab sdg' n of 
                          Just (InstructionNode _) -> True
--                          Just (GlobalVariableInNode _ _) -> True
                          _  -> False
            GlobalVariableC g -> [getNewId topFunId $ valueUniqueId g]
            _ -> [valueUniqueId v]


-----
addAliasEdge :: IntMap Int -> SDGGraphType -> SDGGraphType        
addAliasEdge am = insEdges2 [(vi,ai,DataDepEdge) | (vi,ai) <- IM.toList am]

genAliasMap2 :: Module -> IntMap Int
genAliasMap2 m = IM.fromList $ glbAlias ++ concatMap getFunAlias2 fs
  where 
    glbAlias = [(- valueUniqueId ga, valueUniqueId $ globalAliasTarget ga)
               | ga <- moduleAliases m ]
    fs = moduleDefinedFunctions m

getFunAlias2 :: Function -> [(Int,Int)]
getFunAlias2 f = mapBoth toArgOutId toFUseId $ getFunAlias' f
  where  toFUseId v = valueUniqueId v - div maxNumID 2
         toArgOutId v = if isGlobal v 
               then -(getNewId fID $ valueUniqueId v)
               else -(valueUniqueId v)
         fID = functionUniqueId f 

---



