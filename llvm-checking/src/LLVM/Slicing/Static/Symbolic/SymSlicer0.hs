{-# LANGUAGE BangPatterns,ViewPatterns,DeriveGeneric,RankNTypes,TemplateHaskell,CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields -rtsopts #-}

module LLVM.Slicing.Static.Symbolic.SymSlicer0 (
 -- * Types
 SliceSummary(..),   -- SliceAnalysis(..),
 -- * Slice Computing
 computeSlice, identifySlice,
 genSliceTable, genSliceTable2,
 getSliceTable,
 -- * Instruction Dependences
 getSrcLnDepTable,getSrcLnDepTable2
 )
 where

import GHC.Generics ( Generic )
import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Lens', makeLenses, (.~), (%~), (^.) )

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import Data.Set ( Set )
import qualified Data.Set as S
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.Maybe 
--import Data.Monoid
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.List ( foldl' ) 
import Data.Char ( isLetter )
--import qualified Data.Text as T (unpack)

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
import LLVM.Slicing.Static.Symbolic.SymADT0

import qualified LLVM.Slicing.Data.Relation as R



------
data SliceSummary =
  SliceSummary {  -- _bwdSliceTable :: SliceTable,
                  -- _procSliTblSumm :: IntMap SliceTable,
                  _procValueDepSumm :: IntMap (IntMap IntSet)
                , _valueDepSummary :: !(IntMap IntSet)
--                , _fwdSliceTable :: SliceTable
                , _traceSize :: Int
                }
  deriving (Generic,Show)

$(makeLenses ''SliceSummary)

instance Eq SliceSummary where
  (SliceSummary pvd1 vdm1 _) == (SliceSummary pvd2 vdm2 _) =
      pvd1 == pvd2 && vdm1 == vdm2  

instance Semigroup SliceSummary where
  (SliceSummary pvd1 vdm1 tr1) <> (SliceSummary pvd2 vdm2 tr2) =
    SliceSummary (IM.unionWith mrgValDep pvd1 pvd2)
                 (mrgValDep vdm1 vdm2)(tr1 + tr2)  

instance Monoid SliceSummary where
  mempty = SliceSummary mempty mempty 0 
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif

instance NFData SliceSummary where
  rnf = genericRnf



--
data SliceAnalysis = SliceAnalysis { _sliceSumm :: SliceSummary }
  deriving (Eq, Generic)

$(makeLenses ''SliceAnalysis)

instance NFData SliceAnalysis where
  rnf = genericRnf

instance Semigroup SliceAnalysis where
  a1 <> a2 =
    SliceAnalysis { _sliceSumm = _sliceSumm a1 <> _sliceSumm a2 }

instance Monoid SliceAnalysis where
  mempty = SliceAnalysis { _sliceSumm = mempty }
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif

---------------
-----
top :: SliceInfo
top = SInfo mempty

meetSliceInfo :: SliceInfo -> SliceInfo -> SliceInfo
meetSliceInfo (SInfo !vd1) (SInfo !vd2) = {-# SCC meetSliceInfo #-}
          SInfo (mrgValDep vd1 vd2)   
{-# INLINE meetSliceInfo #-}

sliceTransfer :: SliceInfo -> Instruction -> Analysis SliceInfo
sliceTransfer si i = {-# SCC sliceTransfer #-} do 
  cdM <- analysisEnvironment instCtrMap
  addTrace i
  case i of    
    StoreInst {storeAddress = ptr, storeValue = sv} -> 
       updSliInfo i (ptr,sv) si cdM 
    AtomicRMWInst {atomicRMWPointer = ptr, atomicRMWValue = av} ->
      updSliInfo i (ptr,av) si cdM
    AtomicCmpXchgInst {atomicCmpXchgPointer = ptr, atomicCmpXchgNewValue = nv} ->
      updSliInfo i (ptr,nv) si cdM
      
    InsertValueInst {insertValueAggregate = a, insertValueValue = iv} -> 
      updSliInfo i (a,iv) si cdM
----    PhiNode {} -> updSliInfo i (toValue i, toValue i) si cdM
      
    CallInst { callFunction = fv, callArguments = avs } ->
      callTransfer cdM si i fv (map fst avs)
    InvokeInst { invokeFunction = fv, invokeArguments = avs } ->
      callTransfer cdM si i fv (map fst avs) 
      
    _ -> setTrValueDep cdM si i     
{-# INLINE sliceTransfer #-}

updSliInfo :: Instruction -> (Value,Value) -> SliceInfo -> IntMap ValueIds -> Analysis SliceInfo
updSliInfo i (ptr,v) si cdM  =  -- dbgIt i $ 
  case valueContent ptr of      
    InstructionC PhiNode {phiIncomingValues = (map fst -> ivs)} -> do
       let ptValIDs = instRefs ptr
           si2 = xtdValueDep2 ptValIDs l' si
       addTrValueDep si2 i l'
    InstructionC SelectInst {selectTrueValue = tv, selectFalseValue = fv} -> do
       let ptValIDs = IS.fromList $ map (valueUniqueId. memAccessBase) [tv,fv]
           si2 = xtdValueDep2 ptValIDs l' si
       addTrValueDep si2 i l'
    _ -> addTrValueDep si' i l'
  where
    !l' = unionLs cdM si i v
    base = memAccessBase ptr
    baseID = valueUniqueId base
    updOrXtdValDep = if isAggregate ptr || isAggType base
                     then xtdValueDep else updValueDep
    si' = updOrXtdValDep baseID l' si 


callTransfer :: IntMap ValueIds -> SliceInfo -> Instruction -> Value -> [Value] -> Analysis SliceInfo
callTransfer cdM si i fv cargs = {-# SCC callTransfer #-} do
--  ptM <- analysisEnvironment procSliTbl
  pdM <- analysisEnvironment procValueDep
  paraMap <- analysisEnvironment paraValMap
  case valueContent' fv of
    FunctionC f -> do
      let fID = functionUniqueId f
          fFmlIds = map valueUniqueId (functionParameters f) 
          fValDep = IM.findWithDefault IM.empty fID pdM
          argMap = IM.filterWithKey mapF paraMap
             where mapF n (v,k) = k == -1 || elem n fFmlIds                   
          isArgIn v = lkpValueDep (memAccessBase v) fValDep == IS.singleton (- valueUniqueId v)
          noChgArgs = [valueUniqueId v | (v,k) <- IM.elems argMap, isArgIn v, k /= -1]
          noChgGlbs = [valueUniqueId v | (v,k) <- IM.elems argMap, isArgIn v, k == -1]
          iID = instructionUniqueId i 
          glbIds = IM.keys $ IM.filter ((== -1). snd) paraMap
          iCtrDep = unionLs cdM si i fv          
          --
          fValDep' = IM.mapWithKey fillF fValDep 
             where fillF var !lx 
                     | elem var noChgGlbs = IS.empty
                     | elem var noChgArgs = iCtrDep 
                     | otherwise  =  IS.unions $ [lx1,iCtrDep] ++ lxs    
                     where (lx1,lx2) = IS.partition (>0) lx
                           lxs = map (lci. negate) $ IS.toList lx2     
                   lci n = case IM.lookup n argMap of                
                            Just (gv,-1) -> unionLs cdM si i gv
                            Just (v,k) -> unionLs cdM si i (cargs !! k)
                            _  -> IS.singleton (-n)
          chgActArgs = [(valueUniqueId (toActVar v k), lkpValueDep v fValDep') 
                        | (v,k) <- IM.elems argMap, not (isArgIn v)] 
             where toActVar v k = if k == -1 then v else memAccessBase (cargs !! k) 
          si' = updValDeps chgActArgs $! (valueDepMap %~ mrgValDepWith glbIds fValDep') si 
--          dbgStr = "  \n" ++ show fID ++ "'s oldValDep = " ++ show fValDep ++
--                   "  \n" ++ show fID ++ "'s procValDep'= " ++ show fValDep'
--          si2 = setValueDep fValDep' si'  
      addTrValueDep si' i iCtrDep   -- `debug` dbgStr    
    ExternalFunctionC ef      
      | isMemCMS ef   -> updSliInfo i (cargs!!0,cargs!!1) si cdM
      | isC99Scanf ef -> updSliInfo i (cargs!!1,cargs!!0) si cdM
      | isC99Read ef  -> updSliInfo i (cargs!!0,undef) si cdM
      | otherwise     -> setTrValueDep cdM si i 
      where  undef = ConstantC UndefValue {constantType=TypeVoid, constantUniqueId = 0}
    _ -> setTrValueDep cdM si i  
{-# INLINE callTransfer #-}

identifySlice ::    
       (FuncLike funcLike, HasFunction funcLike, HasCFG funcLike)
        => Module -> Lens' compositeSummary SliceSummary
        -> ComposableAnalysis compositeSummary funcLike
identifySlice m lns =
  composableAnalysisM runner (sliceAnalysis m) lns
  where
    runner a = runAnalysis a constData cache
    constData = SEnv mempty mempty mempty 
    cache = SState 0   -- mempty


sliceAnalysis :: (FuncLike funcLike, HasCFG funcLike,HasFunction funcLike)
               => Module -> funcLike -> SliceSummary -> Analysis SliceSummary
sliceAnalysis m funcLike s@(SliceSummary pdM _ _) = do   
  let envMod e = e { procValueDep = pdM
                   , paraValMap = IM.fromList $ zip allParaIds allParaVals  
                   , instCtrMap = genCtrDepMap f  
                   }
      fact0 = xtdValDeps initSymSli top
      analysis = fwdDataflowAnalysis top meetSliceInfo sliceTransfer  -- dataflowAnalysis
  localInfo <- analysisLocal envMod (dataflow funcLike analysis fact0)   -- forwardDataflow
  SState tr <- analysisGet 
  let trStr = "\n" ++ fName ++ fStrLn ++ "\'s #Insts_traced = " ++ show tr 
      fStrLn = case getFunctionLine f of
               Just (src, ln) -> "(Defined at " ++ src ++ ":" ++ show ln ++ ")"
               Nothing        -> ""  
      fName = identifierAsString (functionName f)
      SInfo ideps = dataflowResult localInfo
--      !valMap = genValueMap m
--      fs' = getFwdSlices1 valMap ideps fs 
  return  --- $ (procSliTblSumm %~ IM.insertWith' mrgSli fID bs')      
         $ (procValueDepSumm %~ IM.insertWith (mrgValDepWith glbIds) fID ideps) -- (\_ y -> y)    
--         $ (bwdSliceTable %~ mrgSli bs')         
--         $ (fwdSliceTable %~ mrgSli fs')
         $ (traceSize %~ (+) tr)
         $! (valueDepSummary %~ mrgValDepWith glbIds ideps) s  --  `debug` trStr
  where
    f = getFunction funcLike 
    fID = functionUniqueId f 
    initSymSli = map initSlice fParaVals
      where initSlice (v,n) 
              | n == -2   =  (valueUniqueId v, IS.empty) 
              | otherwise =  (valueUniqueId v, IS.singleton $ - valueUniqueId v) 
    fParaIds = map (valueUniqueId . fst) fParaVals        
    fParaVals = frmlVals f ++ globalVals ++ allocVals f
    --
    allParaIds = map (valueUniqueId . fst) allParaVals
    allParaVals = globalVals ++ (concatMap frmlVals $ moduleDefinedFunctions m)
    frmlVals fn = zip (map toValue $ functionParameters fn) [0..]
    glbIds = map (valueUniqueId . fst) globalVals
    globalVals = zip (map toValue $ moduleGlobalVariables m) (repeat (-1))
    allocVals fn = zip (map toValue $ funcAllocInsts fn) (repeat (-2))

----
type IsParallel = Bool
computeSlice :: IsParallel -> Module -> SliceSummary
computeSlice isPar m = _sliceSumm res    -- `showGraph` (cg,mName)
  where
    cg = callGraph m ics []
    ics = runPointsToAnalysis m
--    mName = T.unpack $ moduleIdentifier m
    analyses :: [ComposableAnalysis SliceAnalysis Function]
    analyses = [ identifySlice m sliceSumm ]
    analysisFunc = callGraphComposeAnalysis analyses   
    res = cgTraversal cg analysisFunc mempty      
    cgTraversal = if isPar then parallelCallGraphSCCTraversal else callGraphSCCTraversal

-------------
genSliceTable2 :: IsParallel -> Module -> (Map String [String],Map String [String])
genSliceTable2 isPar m = (toSrcLns bwdST, toSrcLns fwdST)
  where (bwdST,fwdST) = genSliceTable isPar m
        valMap = genValueMap m
        toSrcLns = M.map (toSrcLnStr valMap) 

genSliceTable :: IsParallel -> Module -> (SliceTable,SliceTable)    
genSliceTable isPar m = getSliceTable summary valMap m
  where !summary = computeSlice isPar m
        valMap = genValueMap m    
        
getSliceTable :: SliceSummary -> IntMap Value -> Module -> (SliceTable,SliceTable)    
getSliceTable summary valMap m = (bwdSlices, fwdSlices) -- `debug` trStr
  where valDepM = _valueDepSummary summary
        aliasMap = genAliasMap1 m
        fs = moduleDefinedFunctions m
        allVals =  map toValue (concatMap functionParameters fs)
                ++ map toValue (moduleGlobalVariables m) 
                ++ map toValue (concatMap funcAllocInsts fs)
        allVals' = filter ((\(k:_)->(isLetter k || k == '_')). drop 1. toVarName') allVals
        varIds = map valueUniqueId allVals'
        vars = map (drop 1 . toVarName') $ allVals'
        addAlia n = n : (maybeToList $ IM.lookup n aliasMap)        
        bwdSlices = M.fromList. zip vars $ map (unionLkpValDep' valDepM. addAlia) varIds
--        fwdSlices = M.fromList. zip vars $! map (getFwdSlices2 valDepM. addAlia) varIds
        aliasMap2 = genAliasMap m
        fwdSlices = reduceKey. addAlias aliasMap2. getFwdSlices valMap $ valDepM        
        reduceKey = M.mapKeys (drop 1). M.filterWithKey (\(_:k:_) _->(isLetter k || k == '_'))
        trStr = "\n\tIts trace Info: #Insts_Symbolic = " ++ show (_traceSize summary)

addAlias :: Map String String -> SliceTable -> SliceTable
addAlias am s = updSlices aliNames aliSlices s
  where  
    (aliNames,aliSlices) = unzip . concatMap fromAlias $ M.toList am
    fromAlias (v1,v2) = [(v1,unionSli),(v2,unionSli)] 
      where unionSli = IS.union (lkpSli v1 s) (lkpSli v2 s)
 
--------------
---
getFwdSlices2 :: IntMap ValueIds -> [Int] -> ValueIds
getFwdSlices2 vdm ns = IS.fromList $ concatMap getFwdSli ns
  where  getFwdSli n = IM.keys $! IM.filter (IS.member n) vdm
        
getFwdSlices :: IntMap Value -> IntMap ValueIds -> SliceTable
getFwdSlices valMap idepMap =  {-# SCC getFwdSlices #-} 
    M.mapWithKey mapF fwdSlices'
  where 
    fwdSlices' = IM.foldlWithKey' doInvert mempty idepMap
    doInvert !acc i !ls = 
      foldl' (\a v -> M.insertWith IS.union v (IS.singleton i) a) acc (toVars ls)    
    toVars = concatMap refs . findVals valMap 
    mapF k !ls = foldl' (instLine k) IS.empty $ findVals valMap ls
    instLine k !ls i = case valueContent' i of 
       InstructionC RetInst {retInstValue = Nothing } -> ls
       InstructionC RetInst {retInstValue = Just (valueContent -> ConstantC{})} -> ls
       InstructionC UnconditionalBranchInst { } -> ls 
       InstructionC UnreachableInst { } -> ls
       InstructionC FenceInst { } -> ls
       _    ->  IS.insert (valueUniqueId i) ls 
{-# INLINE getFwdSlices #-}


----------------------------------------------
---
getSrcLnDepTable2 :: Module -> Map String [String]
getSrcLnDepTable2 m = srcLnDepMap
  where instDepMap = _valueDepSummary $ computeSlice False m
        valMap = genValueMap m
        srcLnDepMap = instDep2SrcLnDep valMap instDepMap

getSrcLnDepTable :: Module -> IntMap IntSet
getSrcLnDepTable m = srcLnDepMap
  where instDepMap = _valueDepSummary $ computeSlice False m
        valMap = genValueMap m
        srcLnDepMap = instDep2SrcLnDep' valMap instDepMap        
        
        instInflRel = R.inv. reduceRel $ imis2r' instDepMap
        imis2r' vm = S.unions [R.mkRelNeighbors a (IS.toList $ IS.filter isValid bs)
                            | (a,bs) <- IM.toList vm, isValid a]
        isValid = isValidInst . findVal valMap
        -- 
        srcInflRel = R.inv. reduceRel $ instDep2SrcDepRel valMap instDepMap   
        --    
        showInstRel = R.printGraphWith (show) (const $ toLabel) "inst_LDG" 
        toLabel i = R.quote $ show (valueLine $ valMap ^! i) ++ ":" ++ show (valMap ^! i)





    


