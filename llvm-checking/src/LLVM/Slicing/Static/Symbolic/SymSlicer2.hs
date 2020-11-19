{-# LANGUAGE BangPatterns,ViewPatterns,DeriveGeneric,RankNTypes,TemplateHaskell,CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields -rtsopts #-}

module LLVM.Slicing.Static.Symbolic.SymSlicer2 (
 -- * Types
 SliceSummary(..),
 -- * Slice Computing
 computeSlice,
 genSliceTable, genSliceTable2,
 -- * Instruction Dependences
-- getInstDepTable,getInstDepTable'
 )
 where

import GHC.Generics ( Generic )
import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Lens', makeLenses, (.~), (%~), (^.) )

import Data.Map ( Map )
import qualified Data.Map as M
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
import Data.List ( foldl', partition ) 
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
import LLVM.Slicing.Static.Symbolic.SymADT2
import qualified LLVM.Slicing.Data.BDD as BDD

import qualified LLVM.Slicing.Data.Relation as R

--import Debug.Trace ( trace )

------
data SliceSummary =
  SliceSummary {  -- _bwdSliceTable :: SliceTable,
                  -- _procSliTblSumm :: IntMap SliceTable,
                  _procValueDepSumm :: IntMap (IntMap ValIdSet)
                , _valueDepSummary :: !(IntMap ValIdSet)
--                , _fwdSliceTable :: SliceTable
                , _traceSize :: Int
                }
  deriving (Generic,Show)

$(makeLenses ''SliceSummary)

instance Eq SliceSummary where
  (SliceSummary pvd1 vdm1 _) == (SliceSummary pvd2 vdm2 _) =
      pvd1 == pvd2 && vdm1 == vdm2  

--instance Monoid SliceSummary where
--  mempty = SliceSummary mempty mempty 0 
--  mappend (SliceSummary pvd1 vdm1 tr1) (SliceSummary pvd2 vdm2 tr2) =
--    SliceSummary (IM.unionWith mrgValDep pvd1 pvd2)
--                 (mrgValDep vdm1 vdm2)(tr1 + tr2) 

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
sliceTransfer si i = {-# SCC sliceTransfer #-}  do     
  cdM <- analysisEnvironment instCtrMap
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

updSliInfo :: Instruction -> (Value,Value) -> SliceInfo -> IntMap ValIdSet -> Analysis SliceInfo
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


callTransfer :: IntMap ValIdSet -> SliceInfo -> Instruction -> Value -> [Value] -> Analysis SliceInfo
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
          isArgIn v = lkpValueDep (memAccessBase v) fValDep == BDD.toBDD (- valueUniqueId v)
          noChgArgs = [valueUniqueId v | (v,k) <- IM.elems argMap, isArgIn v, k /= -1]
          noChgGlbs = [valueUniqueId v | (v,k) <- IM.elems argMap, isArgIn v, k == -1]
          iID = instructionUniqueId i 
          glbIds = IM.keys $ IM.filter ((== -1). snd) paraMap
          iCtrDep = unionLs cdM si i fv          
          --
          fValDep' = IM.mapWithKey fillF fValDep 
             where fillF var !lx 
                     | elem var noChgGlbs = BDD.makeFalse
                     | elem var noChgArgs = iCtrDep
                     | otherwise  =  BDD.mrgBDDs $ [BDD.toBDD lx1,iCtrDep] ++ lxs    
                     where (lx1,lx2) = partition (>0) (BDD.fromBDD lx :: [Int])
                           lxs = map (lci. negate) lx2     
                   lci n = case IM.lookup n argMap of                
                            Just (gv,-1) -> unionLs cdM si i gv
                            Just (v,k) -> unionLs cdM si i (cargs !! k)
                            _  -> BDD.toBDD (-n)
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
                   , instCtrMap = IM.map BDD.toBDD $ genCtrDepMap f  
                   }
      fact0 = xtdValDeps initSymSli top
      analysis = fwdDataflowAnalysis top meetSliceInfo sliceTransfer  -- fwdDataflowAnalysis
  localInfo <- analysisLocal envMod (dataflow funcLike analysis fact0)   -- dataflow
  SState tr <- analysisGet 
  let trStr = "\n" ++ fName ++ fStrLn ++ "\'s #Insts_traced = " ++ show tr 
      fStrLn = case getFunctionLine f of
               Just (src, ln) -> "(Defined at " ++ src ++ ":" ++ show ln ++ ")"
               Nothing        -> ""  
      fName = identifierAsString (functionName f)
      SInfo ideps = dataflowResult localInfo
--      !valMap = genValueMap m
--      fs' = getFwdSlices1 valMap ideps fs 
  return  -- $ (procSliTblSumm %~ IM.insertWith' mrgSli fID bs')      
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
              | n == -2   =  (valueUniqueId v, BDD.makeFalse) 
              | otherwise =  (valueUniqueId v, BDD.toBDD $ - valueUniqueId v) 
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
computeSlice :: Module -> SliceSummary
computeSlice m = _sliceSumm res    -- `showGraph` (cg,mName)
  where
    cg = callGraph m ics []
    ics = runPointsToAnalysis m
--    mName = T.unpack $ moduleIdentifier m
    analyses :: [ComposableAnalysis SliceAnalysis Function]
    analyses = [ identifySlice m sliceSumm ]
    analysisFunc = callGraphComposeAnalysis analyses   
    res = callGraphSCCTraversal cg analysisFunc mempty       -- parallelCallGraphSCCTraversal


-------------
genSliceTable2 :: Module -> (Map String [String],Map String [String])
genSliceTable2 m = (toSrcLns bwdST, toSrcLns fwdST)
  where (bwdST,fwdST) = genSliceTable m
        valMap = genValueMap m
        toSrcLns = M.map (toSrcLnStr valMap) 

genSliceTable :: Module -> (SliceTable,SliceTable)    
genSliceTable m = (bwdSlices, fwdSlices)  
  where valDepM = _valueDepSummary $! computeSlice m
        aliasMap = genAliasMap1 m
        valMap = genValueMap m    
        fs = moduleDefinedFunctions m
        allVals =  map toValue (concatMap functionParameters fs)
                ++ map toValue (moduleGlobalVariables m) 
                ++ map toValue (concatMap funcAllocInsts fs)
        allVals' = filter ((\(k:_)->(isLetter k || k == '_')). drop 1. toVarName') allVals
        varIds = map valueUniqueId allVals'
        vars = map (drop 1 . toVarName') $ allVals'
        addAlia n = n : (maybeToList $ IM.lookup n aliasMap)        
        bwdSlices = M.fromList. zip vars $ map (BDD.fromBDD. unionLkpValDep' valDepM. addAlia) varIds
--        fwdSlices = M.fromList. zip vars $! map (getFwdSlices2 valDepM. addAlia) varIds
        aliasMap2 = genAliasMap m
        fwdSlices = reduceKey. addAlias aliasMap2. getFwdSlices valMap $ valDepM        
        reduceKey = M.mapKeys (drop 1). M.filterWithKey (\(_:k:_) _->(isLetter k || k == '_'))
        


addAlias :: Map String String -> SliceTable -> SliceTable
addAlias am s = updSlices aliNames aliSlices s
  where  
    (aliNames,aliSlices) = unzip . concatMap fromAlias $ M.toList am
    fromAlias (v1,v2) = [(v1,unionSli),(v2,unionSli)] 
      where unionSli = IS.union (lkpSli v1 s) (lkpSli v2 s)
 
--------------
---
getFwdSlices2 :: IntMap ValIdSet -> [Int] -> ValueIds
getFwdSlices2 vdm ns = IS.fromList $ concatMap getFwdSli ns
  where  getFwdSli n = IM.keys $! IM.filter (BDD.elemBDD n) vdm
        
getFwdSlices :: IntMap Value -> IntMap ValIdSet -> SliceTable
getFwdSlices valMap idepMap =  {-# SCC getFwdSlices #-} 
    M.mapWithKey mapF fwdSlices'
  where 
    fwdSlices' = IM.foldlWithKey' doInvert mempty (IM.map BDD.fromBDD idepMap)
    doInvert !acc i !ls = 
      foldl' (\a v -> M.insertWith' IS.union v (IS.singleton i) a) acc (toVars ls)    
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
{-
getInstDepTable :: Module -> Map String [String]
getInstDepTable m = getInstDepTable' valMap $ computeSlice m
  where valMap = genValueMap m

getInstDepTable' :: IntMap Value -> SliceSummary -> Map String [String]
getInstDepTable' valMap summ = srcLnDepMap
  where instDepMap = _valueDepSummary summ
        instInflRel = R.inv. reduceRel $ imis2r' instDepMap
        imis2r' vm = S.unions [R.mkRelNeighbors a (IS.toList $ IS.filter isValid bs)
                            | (a,bs) <- IM.toList vm, isValid a]
        isValid = isValidInst . findVal valMap
        -- 
        srcLnDepMap = instDep2SrcLnDep valMap instDepMap
        srcInflRel = R.inv. reduceRel $ instDep2SrcDepRel valMap instDepMap   
        --    
        showInstRel = R.printGraphWith (show) (const $ toLabel) "inst_LDG" 
        toLabel i = R.quote $ show (valueLine $ valMap ^! i) ++ ":" ++ show (valMap ^! i)
-}
    

     


      


    


