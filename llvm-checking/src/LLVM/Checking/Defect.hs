{-# LANGUAGE DeriveGeneric, TemplateHaskell, ViewPatterns, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, KindSignatures, FlexibleInstances,
             ExistentialQuantification, GADTs, CPP, UndecidableInstances #-}
-- | An analysis to identify program defect from some rules.
--

module LLVM.Checking.Defect (
  DefectSummary(..),
  identifyDefects,
  chkOtherOldRules,
  -- * Utils
  getFldFinalIdx, getElemValFinalIdx,
  getAPfinalTags, getFinalArrSize, getFinalFldSizeIdx,
  -- * Testing
  toJsonString, writeJsonFile,
  showDefectSumm,
  defectSummaryToTestFormat
  ) where

import GHC.Generics ( Generic )

import Control.Arrow  -- ( (***) )
import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Getter, Lens', (%~), (.~), (^.), makeLenses, view, to )
import Control.Monad ( foldM, liftM, liftM2 )
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import qualified Data.Foldable as F
import Data.Maybe  -- ( fromMaybe, mapMaybe )
import Safe.Failure ( at )
--import Data.Monoid
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM

import qualified Data.ByteString.Lazy.Char8  as LBS
import Data.Aeson as A  (FromJSON, ToJSON, encode, toJSON)
--import qualified Data.Aeson.Types as A

import qualified Data.Text as T
import Data.List -- ( foldl',find, (\\), union, partition, nub, stripPrefix, isInfixOf, isPrefixOf )
import Data.Generics.Uniplate.Data (universeBi)
import qualified ABI.Itanium as ABI


import LLVM.Parse

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.CDG
import LLVM.Analysis.CallGraph
import LLVM.Analysis.Dominance
import LLVM.Analysis.ClassHierarchy
import LLVM.Analysis.BlockReturnValue
import LLVM.Analysis.NoReturn
import LLVM.Analysis.CallGraphSCCTraversal
import LLVM.Analysis.UsesOf
import LLVM.Analysis.Util.Testing
import LLVM.Analysis.Util.Names
import qualified LLVM.Analysis.PointsTo.TrivialFunction as PT
import LLVM.Analysis.PointsTo.Andersen
import LLVM.Analysis.PointsTo
import LLVM.Analysis.AccessPath
import LLVM.Analysis.NullPointers
import LLVM.Analysis.Dataflow

import Foreign.Inference.AnalysisMonad
import Foreign.Inference.Diagnostics
import Foreign.Inference.Interface
import Foreign.Inference.Interface.Types
--import Foreign.Inference.Interface.Metadata
import Foreign.Inference.Analysis.Util.CalleeFold
import Foreign.Inference.Report
import Foreign.Inference.Preprocessing

import LLVM.Checking.Inference.Metadata
import LLVM.Checking.Inference.Allocator  as Al
import LLVM.Checking.Inference.Array  as Ar
import LLVM.Checking.Inference.ErrorHandling  as EH
import LLVM.Checking.Inference.Escape   as Es
import LLVM.Checking.Inference.Finalize  as Fi
import LLVM.Checking.Inference.Nullable  as Nu
import LLVM.Checking.Inference.Output  as Out
import LLVM.Checking.Inference.RefCount  as RC
import LLVM.Checking.Inference.Return  as Re
import LLVM.Checking.Inference.SAP  as Sap
import LLVM.Checking.Inference.SAPPTRel as SP
import LLVM.Checking.Inference.ScalarEffects  as SE
import LLVM.Checking.Inference.Transfer  as Tr
import LLVM.Checking.Inference.IndirectCallResolver  as ICR
--import LLVM.Checking.CheckSummary  as ChkSumm
import LLVM.Checking.Inference.INullPtrs as INP
import LLVM.Checking.Inference.SMT  hiding ( isSat,ignoreCasts )

import LLVM.Slicing 
import LLVM.Slicing.Static.Symbolic.BwdSymSli as Bwd
import LLVM.Slicing.Static.Symbolic.SymSlicer0 as Sym
import LLVM.Slicing.Static.InfoFlow.InfoFlowSlicer as IF

import LLVM.Executing.SymbolicExe as SymE
import LLVM.Executing.SymExeType as SymE  hiding ( top ) 

import Data.SBV
-- import Debug.Trace
-- debug = flip trace

type SummaryType = IntMap (HashSet Value)

data DefectReport = DefectReport 
    { totalDefects  :: Int
    , defects :: [DefectItem] 
    } deriving (Show, Generic)
data DefectItem = DefectItem 
    { defectType   :: Int
    , fileName    :: FilePath
    , line        :: Int
    } deriving (Show, Generic)

instance FromJSON DefectReport
instance FromJSON DefectItem
instance ToJSON DefectReport
instance ToJSON DefectItem

data DefectSummary = DefectSummary {
  _defectSummary :: SummaryType,
  _defectDiagnostics :: Diagnostics
  }
  deriving (Generic)

$(makeLenses ''DefectSummary)

showDefectSumm :: SummaryType -> String
showDefectSumm  dm  = showMapWith2 header id id sm
  where 
    header = "\n DefectTypes           SrcLineNumbers"
    sm = mapBoth keyF valF $ IM.toAscList dm
    keyF k = "DefectType_" ++ show k
    valF  = toSrcLnStr' . HS.toList 
    
summToReport ::  SummaryType -> DefectReport
summToReport dm  = DefectReport (length dfs) dfs
  where 
    dfs = [DefectItem k fp ln | (k,vs) <- IM.toAscList dm, 
              (fp,lns) <- mapMaybe valueSrcLn' $ HS.toList vs, ln <- lns ] 

toJsonString :: SummaryType -> LBS.ByteString
toJsonString = A.encode . toJSON . summToReport

writeJsonFile :: FilePath -> SummaryType -> IO ()
writeJsonFile fp = LBS.writeFile fp . toJsonString  


instance Eq DefectSummary where
  (DefectSummary as1 _) == (DefectSummary as2 _) = as1 == as2

instance Show DefectSummary where
  show (DefectSummary dm _) = showDefectSumm dm

instance Semigroup DefectSummary where
  (DefectSummary t1 d1) <> (DefectSummary t2 d2) =
    DefectSummary (IM.unionWith HS.union t1 t2) (d1 <> d2)

instance Monoid DefectSummary where
  mempty = DefectSummary mempty mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif
instance NFData DefectSummary where
   rnf = genericRnf

instance HasDiagnostics DefectSummary where
  diagnosticLens = defectDiagnostics

instance SummarizeModule DefectSummary where
  summarizeFunction _ _ = []
  summarizeArgument _ _ = []

data DefectEnv = DE { 
                     ptaSumm :: IndirectCallSummary
                   , useSumm :: UseSummary   
--                     controlDepGraph :: CDG
--                   , domTree :: DominatorTree
--                   , nullSumm :: NullableSummary
                   , chaSumm :: CHA
                   , enumMap :: Map String [(String,Int)]     --  [CEnum] 
                   , nullVals :: Set Value
                   }

data DefectState = DState { custodialCache :: HashMap Value [Value]
                          , intValueCache :: HashMap Value (Maybe Int)
                          , formulaCache :: IntMap (Maybe (SInt32 -> SBool))
                          , freeCache :: Set Value }

data DefectInfo = DI {
       _defectRuleSumm :: SummaryType        --- IntMap (HashSet Value)
     }  deriving (Eq,Ord,Show,Generic)

$(makeLenses ''DefectInfo)

type Analysis = AnalysisMonad DefectEnv DefectState
type DefectAnalysis = DefectInfo -> Analysis DefectInfo
type SomeSummaries = ((SymExeSummary, NullableSummary),(SAPPTRelSummary, SAPSummary))
getSymExeSumm = fst . fst
getNullSumm = snd . fst
getSapPTSumm = fst . snd
getSapSumm  = snd . snd

identifyDefects ::  forall compositeSummary funcLike .  
                  (FuncLike funcLike, HasFunction funcLike, HasCFG funcLike, 
                    HasCDG funcLike, HasDomTree funcLike)
                    =>  Module -> DependencySummary
                    -> Lens' compositeSummary DefectSummary
                    -> Getter compositeSummary SymExeSummary
                    -> Getter compositeSummary NullableSummary
                    -> Getter compositeSummary SAPPTRelSummary
                    -> Getter compositeSummary SAPSummary
                    -> ComposableAnalysis compositeSummary funcLike
identifyDefects m ds lns seLens nuLens sptLens sapLens   =   
    composableDependencyAnalysisM runner (defectAnalysis m) lns depLens 
    -- composableAnalysisM runner (defectAnalysis m) lns
  where
    runner a = runAnalysis a ds constData cache
    constData = DE ics uses cha enums undefined
    cache = DState mempty mempty mempty mempty
    enums = genCEnumMap m
    uses = computeUsesOf m
    ics = identifyIndirectCallTargets m 
    cha = runCHA m    
    depLens :: Getter compositeSummary SomeSummaries
    depLens = to ((view seLens &&& view nuLens) &&& (view sptLens &&& view sapLens))

defectAnalysis :: (FuncLike funcLike, HasFunction funcLike, HasCFG funcLike,
                     HasCDG funcLike, HasDomTree funcLike)
                     => Module -> SomeSummaries   -- 
                     -> funcLike
                     -> DefectSummary
                     -> Analysis DefectSummary
defectAnalysis m someSumms funcLike s = do    
  let envMod e = e {  nullVals = fNullVals  -- funcNullPtrs m f
                   }
      analysis = fwdDataflowAnalysis top meet (transfer m someSumms)
  localInfo <- analysisLocal envMod (dataflow funcLike analysis top)
  let DI di = dataflowResult localInfo
  return $! (defectSummary %~ IM.unionWith HS.union di ) s
  where
    f = getFunction funcLike   
    sptSumm = getSapPTSumm someSumms
    svStoreAPs sv = getValuePathsFor sptSumm sv
    svStoreVals sv = map getValFromAP $ svStoreAPs sv
    svStoreArgs sv = map getValFromAP $ synthesizedPathsFor sptSumm sv
    fValPaths = M.findWithDefault M.empty f $ _sapValues sptSumm
    fNullCnsts = filter isNullValue $ M.keys fValPaths
    fNullVals  = S.fromList $ concatMap svStoreVals fNullCnsts

top :: DefectInfo
top = DI mempty 

meet :: DefectInfo -> DefectInfo -> DefectInfo
meet (DI d1) (DI d2) = DI $ IM.unionWith HS.union d1 d2


addToDInfo :: Int -> [Value] -> DefectInfo -> DefectInfo
addToDInfo n ds (DI dis) = DI $ IM.insertWith HS.union n (HS.fromList ds) dis

addOneToSumm n d (DI dis) = DI $ IM.insertWith HS.union n (HS.singleton d) dis

lkpDInfo n (DI dis) = IM.findWithDefault mempty n dis
lkpDInfos ns (DI dis) = HS.unions. IM.elems $ IM.filterWithKey (\k _ -> elem k ns) dis

transfer :: Module 
         -> SomeSummaries
         -> DefectInfo
         -> Instruction
         -> Analysis DefectInfo
transfer m ((sexeSumm,nullSumm),(sptSumm,sapSumm)) di i =  do   
  em <- analysisEnvironment enumMap
  nvs <- analysisEnvironment nullVals
  ics <- analysisEnvironment ptaSumm
  uses <- analysisEnvironment useSumm
  cha <- analysisEnvironment chaSumm
  let SymExeSummary varSbvTbl _ condSbvTbl initSbvTbl = sexeSumm      
      annMap = _nullableSummary nullSumm   -- HashMap Argument [Witness]

      SAPPTRelSummary ptrStoredPaths valStoredPaths _ = sptSumm
      getArgStorePaths = synthesizedPathsFor sptSumm
      getValStorePaths :: IsValue a => a -> [AccessPath]
      getValStorePaths = getValuePathsFor sptSumm
      getAddrVals :: IsValue a => a -> [Value]
      getAddrVals = getLocAddrValue sptSumm

      SAPSummary fRetFrmlPaths argStoredPaths argFinalPaths fRetActual _ = sapSumm
      getArgfRetPaths arg = returnedPaths (argumentFunction arg) arg sapSumm
      getArgWritePaths arg = writePaths arg sapSumm
      getFunRetPaths f = returnedContainerPaths f sapSumm

  di2 <- case i of 
        SwitchInst { switchValue = v, switchDefaultTarget = dt, 
                     switchCases = (map fst -> vs) } -> return di

        BranchInst { branchCondition = c } ->
          checkManyRules di [checkRule5 sexeSumm c i]
          
        LoadInst {loadAddress = la} ->  
          checkManyRules di [checkRule1 m i la, checkRule2 m i la,
                checkRule9 i [la] annMap sptSumm]
            
        StoreInst {storeAddress = ptr, storeValue = sv} -> 
          checkManyRules di [checkRule1 m i ptr, checkRule2 m i ptr, 
                checkRule9 i [ptr,sv] annMap sptSumm, checkRule8 False sexeSumm i, 
                checkRule4 sexeSumm i]

        AtomicRMWInst {atomicRMWPointer = ptr, atomicRMWValue = av} -> 
          checkManyRules di [checkRule1 m i ptr, checkRule2 m i ptr,
                checkRule9 i [ptr,av] annMap sptSumm]
          
        AtomicCmpXchgInst {atomicCmpXchgPointer = ptr, atomicCmpXchgNewValue = nv} -> 
          checkManyRules di [checkRule1 m i ptr, checkRule2 m i ptr, 
                checkRule9 i [ptr,nv] annMap sptSumm]
          
        InsertValueInst {insertValueAggregate = a, insertValueValue = iv} -> 
          checkManyRules di [checkRule1 m i a, checkRule2 m i a, 
                 checkRule9 i [a,iv] annMap sptSumm]
          
        PhiNode {phiIncomingValues = (map fst -> ivs)} -> return di
        
        GetElementPtrInst { getElementPtrValue = base, getElementPtrIndices = ixs } -> 
          checkManyRules di [checkRule1 m i (toValue i), checkRule2 m i (toValue i) ]     
          
        CallInst { callFunction = fv, callArguments = (map fst -> avs) } -> 
          checkManyRules di [checkRule1' m i fv avs, checkRule9 i avs annMap sptSumm, 
             checkRule10 i fv avs, checkRule12 i fv avs]
            
        InvokeInst { invokeFunction = fv, invokeArguments = (map fst -> avs) } -> 
          checkManyRules di [checkRule1' m i fv avs, checkRule9 i avs annMap sptSumm, 
             checkRule10 i fv avs, checkRule7 i fv avs, checkRule12 i fv avs ]
          
        DivInst { binaryLhs = l, binaryRhs = r }  -> 
          checkManyRules di [checkRule6 sexeSumm i r, checkRule8 True sexeSumm i]
        RemInst { binaryLhs = l, binaryRhs = r }  -> 
          checkManyRules di [checkRule6 sexeSumm i r]
        AndInst { binaryLhs = lhs, binaryRhs = rhs } -> 
          checkManyRules di [checkRule8 True sexeSumm i]
        SubInst { binaryLhs = lhs, binaryRhs = rhs } ->   
          checkManyRules di [checkRule8 True sexeSumm i]   
        MulInst { binaryLhs = lhs, binaryRhs = rhs } ->
          checkManyRules di [checkRule8 True sexeSumm i]


        ICmpInst {cmpV1 = v1, cmpV2 = v2} -> return di
          -- checkManyRules di [checkRule5 sexeSumm i]
        FCmpInst {cmpV1 = v1, cmpV2 = v2} -> return di
        
        BitcastInst {castedValue = cv} -> 
           checkManyRules di [checkRule3 sexeSumm i]
          
        
        RetInst {retInstValue = Just rv} -> return di
        
        ResumeInst {resumeException = ev} ->  return di    
        UnreachableInst {} ->  return di
              
        _  ->  checkManyRules di [checkRule8 False sexeSumm i]

  checkManyRules di2 [ checkRuleAny i sptSumm ]


   
--------
checkManyRules :: DefectInfo -> [DefectAnalysis] -> Analysis DefectInfo
checkManyRules = foldM (\d f -> f d) 

-- | Rule1: 数组下标越界
checkRule1' m i fv args di 
  | isStrcpy fv  = checkRule1 m i (args!!1) di 
  | isMemcpy fv  = checkMemcpy m i (args!!0) (args!!1) (args!!2) di
  | otherwise  = return di

checkRule1 m i ptr di = 
   case valueContent ptr of 
     InstructionC GetElementPtrInst {getElementPtrValue = base, getElementPtrIndices = ixs} -> do
        let typeIdx = getFldFinalIdx (memAccessBase base) ixs
            finalAPs = concatMap getAPfinalTags [toValue i, base]
            finalSize = maximum. map (typeSize. fst) $ filter ((== AccessArray). snd) finalAPs
            ixOut = isIxOut base ixs
            iType = freePointerType . valueType $ memAccessBase ptr
        case (ixOut, typeIdx) of
          (True, _) -> case iType of
                TypeArray _ _ -> return di3
                TypeStruct _ _ _ -> return di4
                _   -> return di2     
          (False, Just (ty, idx)) -> do
             loopFormula <- getLoopFormula i  --
             case (loopFormula, ixs, idx) of
               (Just lf, [_], Just n) -> do
                  let formula x = lf x .&& ((x + toSInt32 n) .>= toSInt32 baseSize)
                      baseSize = maximum [typeSize ty, finalSize]
                  if isSat formula 
                     -- `debug` ("(isSat formula, baseSize)= " ++ show (isSat formula, baseSize))
                  then return di2 else return di
               _ -> return di
          _ -> return di
     _ -> return di    
   where 
     di2 = addToDInfo 1 [toValue i, ptr] di
     di3 = addToDInfo 2 [toValue i, ptr] di
     di4 = addToDInfo 3 [toValue i, ptr] di

-- | Rule2: 数组下标访问越界
checkRule2 m i ptr di = 
   case valueContent ptr of 
     InstructionC GetElementPtrInst {getElementPtrValue = base, getElementPtrIndices = ixs} -> do
        let typeIdx = getFldFinalIdx (memAccessBase base) ixs
            iArrSize = getFinalArrSize (toValue i)
            (iFldSize, idx) = getFinalFldSizeIdx (toValue i)
            ixOut = isIxOut base ixs
            iType = freePointerType . valueType $ memAccessBase ptr
        loopFormula <- getLoopFormula i
        case (length ixs > 1, typeIdx, loopFormula) of
          (False, Just (ty, Just n), Just lf) -> do
              let formula1 x = lf x .&& ((x + toSInt32 n) .>= toSInt32 iArrSize)
                  formula2 x = lf x .&& ((x + toSInt32 n) .>= toSInt32 iFldSize)
              case iType of
                TypeArray _ _ -> if isSat formula1 then return di2 else return di
                TypeStruct _ _ _ -> if isSat formula2 then return di3 else return di
                _   -> return di   
          (True, Just (ty, Nothing), Just lf) -> do
              let formula1 x = lf x .&& (x .>= toSInt32 iArrSize)
                  formula2 x = lf x .&& (x .>= toSInt32 iFldSize)
              case iType of
                TypeArray _ _ -> if isSat formula1 then return di2 else return di
                TypeStruct _ _ _ -> if isSat formula2 then return di3 else return di
                _   -> return di   
          _ -> return di
     _ -> return di    
   where 
     di1 = addToDInfo 1 [toValue i, ptr] di
     di2 = addToDInfo 2 [toValue i, ptr] di
     di3 = addToDInfo 3 [toValue i, ptr] di



-- | Rule3: 转换结构体前未校验，直接访问域，造成越界
checkRule3 :: SymExeSummary -> Instruction -> DefectAnalysis
checkRule3 seSumm i di = do
  di2 <- chkFlawFromSymExeWith True 3 seSumm i di
  let (size, idx) = getFinalFldSizeIdx i  
      isOut = idx + 1 > size
  if isOut then return di3 else return di2
 where 
  di3 = addToDInfo 3 [toValue i] di


-- | Rule4: 外部数据直接运算后整数溢出，从而导致指针访问越界
checkRule4 :: SymExeSummary -> Instruction -> DefectAnalysis
checkRule4 (SymExeSummary varSbvTbl _ _ _) i di =   
    if chkOver then return di2 else return di
 where  
    di2 = addToDInfo 4 [toValue i] di
    r8res = lkpDInfo 8 di
    r123res = lkpDInfos [1..3] di
    chkRes = HS.member (toValue i) $ HS.intersection r8res r123res
    lkpOverflow :: IsValue a => a -> Maybe SymExeExpr      
    lkpOverflow v = M.lookup ("!_#8_" ++ toVarName' v) varSbvTbl
    refOverflow = any isOverflow. S.toList. S.unions. 
                  mapMaybe lkpOverflow $ refVars i    
    isOverflow v = case unliteral v of
                  Just a -> a == 1 
                  Nothing -> True
    isUndef = not . HS.null . HS.filter isConstantUndef $ valueRefs i
    chkOver = (chkRes && refOverflow) || isUndef


-- | Rule5: 死循环，因外部数据运算后溢出导致
checkRule5 :: SymExeSummary -> Value -> Instruction -> DefectAnalysis
checkRule5 seSumm@(SymExeSummary varSbvTbl _ _ _) cv i di =  do
   let freePhi = null . filter isPhiNode $ directValueRefs cv
      --  isOne = any (== 1). mapMaybe fromConstantInt. HS.toList $ valueRefs cv
       freeCmp = HS.null . HS.filter isICmpVal $ valueRefs cv
   if freeCmp
   then chkFlawFromSymExeWith False 5 seSumm i di 
   else return di


-- | Rule6: 除零错
checkRule6 :: SymExeSummary -> Instruction -> Value -> DefectAnalysis
checkRule6 (SymExeSummary varSbvTbl _ _ _) i rv di = do
   let iName  = "!_#6_" ++ toVarName' i
       rvSbv  = M.findWithDefault nilSExpr iName varSbvTbl
       chkZero v = case unliteral (forceToIntSbv v) of
                      Just a -> a == 0 
                      Nothing -> True
       chkDiv0 = if S.null rvSbv then False 
                 else any chkZero $ S.toList rvSbv
   if chkDiv0 then return $ addToDInfo 6 [toValue i] di   
   else return di


-- | Rule7: 不可信任数据拼接导致SQL注入
checkRule7 i fv args di =
    if isSQLInject then return di2 else return di
  where
    di2 = addToDInfo 7 [toValue i] di
    isSAString = isPrefixOf "@SAString" . toVarName' $ memAccessBase fv
    isLpsz = any (isPrefixOf "%lpsz") $ mapMaybe toVarName args
    isArgument = any (isJust . asArgument) args 
    isSQLInject = (isLpsz || isArgument) && isSAString
    

-- | Rule8: 运算溢出或反转
checkRule8 :: Bool -> SymExeSummary -> Instruction -> DefectAnalysis
checkRule8 okSym (SymExeSummary varSbvTbl _ _ _) i di = do
   let iName  = "!_#8_" ++ toVarName' i
       rvSbv  = M.findWithDefault nilSExpr iName varSbvTbl
       isOverflow v = case unliteral v of
                      Just a -> a == 1 
                      Nothing -> okSym
       chkOver = if S.null rvSbv then False 
                 else any isOverflow $ S.toList rvSbv
   if chkOver then return $ addToDInfo 8 [toValue i] di   
   else return di
  
-- | Rule9: 空指针解引用
checkRule9 i ptrs annMap sptSumm di = do
  --  uses <- analysisEnvironment useSumm
   let isNullArg arg = null $ HM.lookupDefault [] arg annMap  -- findWithDefault
       ptrVals ptr = ptr : getLocAddrValue sptSumm ptr   -- memAccessBase
       svStoreAPs sv = getValuePathsFor sptSumm sv
       svStoreVals sv = map getValFromAP $ svStoreAPs sv
       svStoreArgs sv = mapMaybe accessPathBaseArgument $ svStoreAPs sv
       isNDeref1 ptr = any isNullValue (ptrVals ptr)
       isNDeref2 v = any isNullArg (svStoreArgs v)
       isNDeref = any (\v -> isNDeref1 v || isNDeref2 v) ptrs
   if isNDeref then return di1 else return di
  where 
   di1 = addToDInfo 9 [toValue i] di
 

 -- | Rule10: 访问已经释放的内存 
checkRule10 i fv avs di = do
  s <- analysisGet 
  let vs = S.fromList $ map memAccessBase avs
      vs2 = S.fromList. filter hasExtraReference $ concatMap refVars avs 
  case ( isFree fv, S.null $ S.intersection vs2 (freeCache s) ) of
    (True, False) -> return $ addToDInfo 10 [toValue i] di
    (True, True) -> do
          analysisPut s { freeCache = S.union vs2 (freeCache s) } 
          return di
    _  -> return di
 
 
-- | Rule12: 对文件路径进行规范化
checkRule12 i fv args di =
    if isRmPath then return di2 else return di
  where
    di2 = addToDInfo 12 [toValue i] di
    isArgument = any (isJust . asArgument) args 
    isRmPath = (isRemove fv) && isArgument



checkRuleAny i sptSumm di = do
  s <- analysisGet 
  let freeVs = freeCache s
      refVs  vs = S.fromList $ concatMap refVars (vs :: [Value])
      refVs2 vs = S.fromList $ concatMap directValueRefs (vs :: [Value])
      defVs vs  = S.fromList $ concatMap defVars (vs :: [Value])
      allVs vs = S.union (defVs vs) $ S.filter hasExtraReference (refVs vs)
  case getInstPtrValue $ toValue i of
    Just v  -> if S.null $ S.intersection (allVs [toValue i, v]) freeVs  
               then return di
               else return $ addToDInfo 10 [toValue i] di
    Nothing -> if S.null $ S.intersection (allVs [toValue i]) freeVs  
               then return di
               else return $ addToDInfo 10 [toValue i] di


chkFlawFromSymExeWith :: Bool -> Int -> SymExeSummary -> Instruction -> DefectAnalysis
chkFlawFromSymExeWith okSym k (SymExeSummary varSbvTbl _ _ _) i di = do
   let iName  = "!_#" ++ show k ++ "_" ++ toVarName' i
       kSbv  = M.findWithDefault nilSExpr iName varSbvTbl
       isFlaw v = case unliteral v of
                    Just a -> a == 1 
                    Nothing -> okSym
       chkSbv = if S.null kSbv then False 
                  else any isFlaw $ S.toList kSbv
   if chkSbv then return $ addToDInfo k [toValue i] di   
   else return di

----------------------------------------------------------------
checkExFunOut :: Module -> Instruction -> Value -> [Value] -> DefectAnalysis
checkExFunOut m i fv args di  
  | isStrcpy fv  = checkRule1 m i (args!!1) di 
  | isMemcpy fv  = checkMemcpy m i (args!!0) (args!!1) (args!!2) di
  | otherwise  = return di

checkMemcpy :: Module -> Instruction -> Value -> Value -> Value -> DefectAnalysis
checkMemcpy m i dest src 
      (valueContent -> ConstantC ConstantInt {constantIntValue = byteCount}) di
  | TypePointer destBaseTy _ <- valueType (stripBitcasts dest)
  , Just tySize <- moduleTypeSizes m destBaseTy
  , tySize /= countSize =
    case isArgument src of
      Just sarg -> if isSat (genFormula sarg) 
                   then return di2 else return di
      Nothing -> if isOut then return di2 else return di
  | otherwise =
    case (isArgument dest, isArgument src) of
      (Just darg, Just sarg) ->  if isSat (genFormula3 darg sarg) 
                                 then return di2 else return di
      (Just darg, Nothing) ->  if isSat (genFormula2 darg) 
                               then return di2 else return di
      (Nothing, Just sarg) ->  if isSat (genFormula sarg) 
                               then return di2 else return di
      _ -> return di
  where
    di2 = addToDInfo 1 [toValue i] di
    (isArgument :: Value -> Maybe Argument) = fromValue . stripBitcasts
    -- iCallers = indirectCallTargets ics i 
    countSize = fromIntegral byteCount
    destSize = valueSize m dest
    srcSize  = valueSize m src
    isOut = countSize > srcSize || countSize > destSize
    genFormula :: IsValue a => a -> (SInt32 -> SBool)
    genFormula sv x =  (x * (toSInt32 $ simpleSize m sv)) .> (toSInt32 destSize)  
    genFormula2 :: IsValue a => a -> (SInt32 -> SBool)
    genFormula2 dv x =  (x * (toSInt32 $ simpleSize m dv)) .< (toSInt32 srcSize)   
    genFormula3 :: Argument -> Argument ->  (SInt32 -> SInt32 -> SBool)
    genFormula3 dv sv x y =  (x * (toSInt32 $ simpleSize m sv)) .> 
                             (y * (toSInt32 $ simpleSize m dv))
checkMemcpy _ _ _ _ _ di = return di


isIxOut base ixs@(ix:xs) = 
  case (valueContent base, fromConstantInt ix) of
    (ConstantC ConstantArray { constantArrayValues = vs }, Just k) -> length vs <= k 
    (ConstantC ConstantStruct { constantStructValues = vs }, Just k) -> length vs <= k 
    -- (v, Just k) -> typeSize (valueType base) <= k
    _ ->  case (valueType $ memAccessBase base, ixs) of
      (TypePointer (TypeArray _ _) _, (valueContent' ->
              ConstantC ConstantInt {constantIntValue = 0 }):_) ->  False
      (TypePointer t _, _:rest) -> goType t rest
      _  -> False
  where 
    goType t [] = False
    goType t [(valueContent -> ConstantC ConstantInt { constantIntValue = (fromIntegral -> k) })] = typeSize t <= k
    goType t (ix:ixs) = case t of
         TypeArray _ t' -> goType t' ixs
         TypeStruct _ ts _ ->
           case valueContent ix of
             ConstantC ConstantInt { constantIntValue = (fromIntegral -> k) } -> k >= length ts 
             _ -> False
         _ -> False


getElemValFinalIdx :: IsValue a => a ->  [(Type, Maybe Int)]
getElemValFinalIdx v = 
   case valueContent v of 
     InstructionC GetElementPtrInst {getElementPtrValue = base, getElementPtrIndices = ixs} ->
          maybeToList $ getFldFinalIdx base ixs
     _  -> concatMap getElemValFinalIdx . filter isGetElem $ refVars v

getFldFinalIdx :: Value -> [Value] -> Maybe (Type, Maybe Int)
getFldFinalIdx base ixs =
  case (valueType $ memAccessBase base, ixs) of
    (_, [valueContent -> ConstantC ConstantInt {constantIntValue = iv}])
             -> return (valueType base, Just $ fromIntegral iv)
    (_, [_]) -> return (valueType base, Nothing)
    (TypePointer (TypeArray _ _) _, (valueContent' -> 
       ConstantC ConstantInt { constantIntValue = 0 }):_) -> return (valueType base, Nothing)
    (TypePointer t _, _:rest) -> walkType t rest
    _ -> Nothing
  where
    walkType :: Type -> [Value] -> Maybe (Type, Maybe Int)
    walkType t [] = Nothing
    walkType t [valueContent -> ConstantC ConstantInt {constantIntValue = iv}] =
        Just (t, Just $ fromIntegral iv)
    walkType t [_] = Just (t, Nothing)
    walkType t (ix:ixs) = case t of
        TypeArray _ t' -> walkType t' ixs
        TypeStruct _ ts _ -> case valueContent ix of
          ConstantC ConstantInt { constantIntValue = (fromIntegral -> iv) } ->
            if iv < length ts then walkType (ts !! iv) ixs else Nothing
          _ -> Nothing
        _ -> Nothing

---
getLoopFormula, getInstFormula :: Instruction -> Analysis (Maybe (SInt32 -> SBool))
getLoopFormula = getInstFormulaWith genLoopFormula
getInstFormula = getInstFormulaWith computeInducedFacts

getInstFormulaWith computeFormula i = do
  st <- analysisGet  
  let iID = valueUniqueId i
      fcache = IM.lookup iID (formulaCache st)
      fbb = do
          f <- instructionFunction i
          bb <- instructionBasicBlock i
          return (f, bb)  
  case (fcache, fbb)  of
    (Just formula, _)       -> return formula
    (Nothing, Just (f,bb) ) -> do      
      let formula = evalState (computeFormula f bb i) (mempty, mempty)
      analysisPut st { formulaCache = IM.insert iID formula (formulaCache st) }
      return formula
    _       -> return Nothing


type FormulaExpr = FormulaBuilder (Maybe (SInt32 -> SBool))
genLoopFormula ::Function -> BasicBlock -> Instruction -> FormulaExpr
genLoopFormula funcLike bb0 target
  | S.null cdeps = return Nothing
  | otherwise = buildRelevantFormulas bb0
  where
    ti0 = basicBlockTerminatorInstruction bb0
    cdeps = S.fromList $ controlDependencies funcLike ti0
    buildRelevantFormulas bb = do
        let ti = basicBlockTerminatorInstruction bb
            dirCdeps = directControlDependencies funcLike ti
        case dirCdeps of
          [] -> return Nothing
          [singleDep] -> cacheFormula bb singleDep
          _ -> do
            fs <- mapM (cacheFormula bb) dirCdeps
            case catMaybes fs of
              [] -> return Nothing
              fs' -> return $ Just $ \(x :: SInt32) -> sAny ($ x) fs'
    cacheFormula :: BasicBlock -> Instruction -> FormulaExpr
    cacheFormula bb cdep = do
      (visited, s) <- get
      case HM.lookup (bb, cdep) s of
        Just f -> return f
        Nothing ->
          case S.member cdep visited of
            True -> return Nothing
            False -> do
              put (S.insert cdep visited, s)
              loopCondFormula bb cdep
    loopCondFormula :: BasicBlock -> Instruction -> FormulaExpr
    loopCondFormula bb cdep = do
      let Just cdepBlock = instructionBasicBlock cdep
          isPhiNode v = case valueContent' v of
                InstructionC PhiNode {} -> True
                _ -> False
      case cdep of
        BranchInst {branchTrueTarget = tt, branchCondition = (valueContent' ->
          InstructionC ICmpInst {cmpPredicate = p, cmpV1 = val1, cmpV2 = val2 })}
            | isPhiNode (ignoreCasts val1)  || isPhiNode (ignoreCasts val2) -> do
                let doNeg = if blockDominates funcLike tt bb then id else sNot
                    thisFormula = inducedFact val1 val2 p doNeg
                innerFormula <- buildRelevantFormulas cdepBlock
                let formula' = liftedConjoin thisFormula innerFormula
                (vis, st) <- get
                put $ (vis, HM.insert (bb, cdep) formula' st)
                return formula'
            | otherwise -> buildRelevantFormulas cdepBlock
        _ -> return Nothing



getEnumValues m v = 
  case null venums of 
    True -> Nothing
    False -> Just enumVals
  where  
     venums = getEnumDef v
     m' = M.filterWithKey filtF m
     filtF k _ = isInfixOf (" " ++ k ++ " ") (head venums) 
     enumVals = head (M.elems m')

fromConstantInt :: Value -> Maybe Int
fromConstantInt v =
  case valueContent v of
    ConstantC ConstantInt { constantIntValue = iv } ->
      return $ fromIntegral iv
    _ -> Nothing

-- getValueSize :: Module -> Value -> Int
-- getValueSize m v = case valueContent' v of
--    ConstantC ConstantArray { constantArrayValues = vs } -> length vs
--    ConstantC ConstantStruct { constantStructValues = vs } -> length vs
--    ConstantC ConstantVector {constantVectorValues = vs} -> length vs
--    _  -> case moduleTypeSizes m vType of 
--           Just tySize  ->  tySize
--           Nothing      ->  typeSize vType
--   where   vType = simpleType $ valueType v

 
asArgument :: Value -> Maybe Argument
asArgument  = fromValue . stripBitcasts


isExtFuns' :: IsValue a => [String] -> a -> Bool
isExtFuns' memFuns v  =
  case valueContent' (toValue v) of
    ExternalFunctionC ef ->
      any (flip isPrefixOf (show $ getExFunctionName ef)) memFuns
    _ -> False 


---
genCEnumMap :: Module -> Map String [(String,Int)]
genCEnumMap m = M.fromList $ zip enames evals
  where
    cenums = moduleInterfaceEnumerations m
    enames = map enumName cenums
    evals = map enumValues cenums

----
getFinalFldSizeIdx :: IsValue a => a -> (Int, Int)
getFinalFldSizeIdx v =  getSizeIdx vRefAPs
  -- if null vAPs then getArrSize vRefAPs else getArrSize vAPs
  where
    vRefAPs = mapMaybe getAPfinalTag (refVars v) 
    getSizeIdx aps = case filter (isFieldPath . snd) aps of
        []  -> (0, -1)
        [k] -> (typeSize $ fst k, getFieldDepth $ snd k)
        ks  -> (maximum $ map (typeSize. fst) ks, 
                maximum $ map (getFieldDepth. snd) ks)
    isFieldPath (AccessField _) = True
    isFieldPath _ = False
    getFieldDepth (AccessField n) = n
    getFieldDepth _ = -1

getFinalArrSize :: IsValue a => a -> Int
getFinalArrSize v =  getArrSize vRefAPs
  -- if null vAPs then getArrSize vRefAPs else getArrSize vAPs
  where
    vRefAPs = mapMaybe getAPfinalTag (refVars v) 
    -- vAPs = getAPfinalTags v
    getArrSize aps = case filter ((== AccessArray). snd) aps of
        []  -> 0
        [k] -> typeSize $ fst k
        ks  -> maximum $ map (typeSize. fst) ks

getAPfinalTags :: Value -> [(Type, AccessType)]
getAPfinalTags v =  
  case getAPfinalTag v of
    Just res -> [res]
    Nothing  -> filter (not. isPointerType. fst) $
                mapMaybe getAPfinalTag (refVars v)

getInstPtrValue :: Value -> Maybe Value
getInstPtrValue ptr =  do
   ap <- valueAsAccessPath (memAccessBase ptr)
   let absAP = abstractAccessPath ap
       baseV = accessPathBaseValue ap
   followAccessPath absAP baseV


getValFromAP :: AccessPath -> Value
getValFromAP ap = fromMaybe baseV (followAccessPath absAP baseV)
  where
    absAP = abstractAccessPath ap
    baseV = accessPathBaseValue ap

getLocAddrValue :: IsValue a => SAPPTRelSummary -> a -> [Value]
getLocAddrValue (SAPPTRelSummary pm vm _) a = fromMaybe [toValue a] $ do
  f <- valueFunction a
  vs' <- M.lookup f vm
  ps' <- M.lookup f pm
  let ps = invertMap' ps'
      vs = invertMap' vs'
  ap <- valueAsAccessPath (toValue a)
  findValFromPath vs ps ap
  where
    findValFromPath vs ps ap = 
      case M.lookup ap vs of
        Just avs  ->  return (S.toList avs)
        Nothing   ->  do
          endValPaths <- M.lookup ap ps
          let res = [findValFromPath vs ps' p| p <- S.toList endValPaths]
              ps' = M.delete ap ps
          return . concat $! catMaybes res

getValuePathsFor :: IsValue a => SAPPTRelSummary -> a -> [AccessPath]
getValuePathsFor (SAPPTRelSummary _ v _) a = fromMaybe [] $ do
  f <- valueFunction a
  vs <- M.lookup f v
  endValPaths <- M.lookup (toValue a) vs
  let res = F.foldr (extendPaths vs) mempty endValPaths
  return (S.toList res)
  where
    extendPaths vs p0 acc
     | S.member p0 acc = acc
     | otherwise = fromMaybe (S.insert p0 acc) $ do
       let base = accessPathBaseValue p0
           vs' = M.delete base vs
       p' <- M.lookup base vs
       return $ F.foldr (extendPath vs' p0) acc p'
    extendPath vs p0 p' acc =
      let ep = p' `appendConcretePath` p0
      in extendPaths vs ep acc

appendConcretePath :: AccessPath -> AccessPath -> AccessPath
appendConcretePath (AccessPath b1 bt1 _ p1) (AccessPath _ _ e2 p2) =
  AccessPath b1 bt1 e2 (p1 ++ p2)

-----
isInjectsNull :: Value -> Bool
isInjectsNull v = isPointer v && case valueContent v of
  InstructionC LoadInst { loadAddress =
    (valueContent -> InstructionC GetElementPtrInst {}) } -> True
  InstructionC LoadInst { loadAddress =
    (valueContent -> InstructionC LoadInst {}) } -> True
  ConstantC ConstantPointerNull {} -> True
  InstructionC BitcastInst { castedValue = cv } -> isInjectsNull cv
  InstructionC PhiNode { phiIncomingValues = ivs } -> any isInjectsNull (map fst ivs)
  _ -> False

isMatchesNull :: Value -> Value -> Bool
isMatchesNull v target = case valueContent v of
  ArgumentC _ -> v == target
  InstructionC LoadInst { loadAddress = la } -> target == la
  InstructionC PhiNode { phiIncomingValues = ivs } -> any (==target) (map fst ivs)
  _ -> False

isNullInitialized :: GlobalVariable -> Bool
isNullInitialized gv = 
   case globalVariableInitializer gv of
      Nothing -> True -- We'll treat uninintialized as null
      Just i -> case valueContent i of
        ConstantC (UndefValue {}) -> True
        ConstantC (ConstantPointerNull {}) -> True
        _ -> False

isNullValue :: Value -> Bool
isNullValue v = case valueContent' v of
    GlobalVariableC gv -> isNullInitialized gv
    -- ConstantC ConstantInt { constantIntValue = 0 } -> isPointerType v
    ConstantC UndefValue {}  -> True
    ConstantC ConstantPointerNull {} -> True
    ConstantC ConstantAggregateZero {} -> True
    _  -> False


---
usedInCondition :: UseSummary -> Instruction -> Bool
usedInCondition useSumm i0 = evalState (go i0) mempty
  where
    go :: Instruction -> State (Set Instruction) Bool
    go i = do
      vis <- get
      case S.member i vis of
        True -> return False
        False -> do
          put $ S.insert i vis
          let uses = usedBy useSumm (toValue i)
              cmpUses = filter isCmp uses
              bitcastUses = filter isBitcast uses
          case cmpUses of
            [] -> do
              res' <- mapM go bitcastUses
              return $ or res'
            _ -> return True
    isCmp i = case i of
        ICmpInst {} -> True
        _ -> False
    isBitcast i = case i of
        BitcastInst {} -> True
        _ -> False

 

-----------------
instRet i = do
  f <- instructionFunction i
  let brs = labelBlockReturns f
  instructionReturn brs i

instRets :: Instruction -> Maybe [Value]
instRets i = do
  f <- instructionFunction i
  let brs = labelBlockReturns f
  instructionReturns brs i


valuesAsInsts :: [Value] -> [Instruction]
valuesAsInsts = mapMaybe fromValue

globalPointerVariables :: Module -> [GlobalVariable]
globalPointerVariables m = filter isPointer (moduleGlobalVariables m)  
  
functionPointerParameters :: Module -> [Argument]
functionPointerParameters m = concatMap pointerParams (moduleDefinedFunctions m)
  where
    pointerParams = filter isPointer . functionParameters  


-----------------------------------
chkOldRule15 i cv di = 
  if isStructType ity then return di2 else return di
  where  
      ity = simpleType $ valueType i
      isDerived = structTypeName ity == "class.Derived"
      di2 = addToDInfo 15 [toValue i, cv] di
    

chkOldRule9 i v1 v2 di = 
  case (isUnsignedVar v1, isUnsignedVar v2) of 
     (True, False) -> return $ addToDInfo 9 [toValue i,v1] di
     (False, True) -> return $ addToDInfo 9 [toValue i,v2] di
     _ -> return di
     

chkOldRule8 i di = do
 case instructionFunction i of
      Just f -> return $ addToDInfo 8 [toValue f] di
      Nothing -> return di 

chkOldRule7 i fv avs di = do
  s <- analysisGet 
  let vs = S.fromList $ map memAccessBase avs
  case ( isFree fv, S.null $ S.intersection vs (freeCache s) ) of
    (True, False) -> return $ addToDInfo 7 [toValue i] di
    (True, True) -> analysisPut s { freeCache = S.union vs (freeCache s) } >> return di
    _  -> return di
    

chkOldRule6 i base ixs di = 
  case getStrcpyIx ixs of 
    Just (n,k) -> if n > k then return di2 else return di
    Nothing -> return di
  where   
    di2 = addToDInfo 6 [toValue i] di
    getStrcpyIx ixs = do 
       ci <- preInst i
       n <-  if length ixs >= 2 then fromConstantInt (ixs !! 1) else Nothing
       case ci of 
         CallInst { callFunction = fv, callArguments = (map fst -> avs) } ->
            if isStrcpy fv && memAccessBase base == memAccessBase (head avs)
            then return (n, getArraySize (avs !! 1)) else Nothing
         InvokeInst { invokeFunction = fv, invokeArguments = (map fst -> avs) } ->            
            if isStrcpy fv && memAccessBase base == memAccessBase (head avs)
            then return (n, getArraySize (avs !! 1)) else Nothing
         _ -> Nothing
    getArraySize = typeSize. valueType. memAccessBase


chkOldRule4' i fv args di = 
  if isStrcpy fv then chkOldRule4 i (args!!1) di else return di 


chkOldRule4 i ptr di = 
   case valueContent ptr of 
     InstructionC GetElementPtrInst { getElementPtrValue = base, getElementPtrIndices = ixs } ->
         if isIxOut base ixs then return di2 else return di
     _ -> return di    
   where di2 = addToDInfo 4 [toValue i, ptr] di

chkOldRule3 i r di = 
   if isConstantZero r || isConstantUndef r then return di2 else return di
  where  di2 = addToDInfo 2 (toValue i : directValueRefs r) di
   

chkOldRule2 i v di nullVs = return di2
   where di1 = addToDInfo 2 (toValue i : directValueRefs v) di
         di2 = if isInjectsNull v || S.member v nullVs then  di1 else di
         
chkOldRule2' i vs di nullVs = return di2
   where vs' = filter (flip S.member nullVs) vs
         di1 = addToDInfo 2 (toValue i : vs') di
         di2 = if null vs' then di else di1 

chkOldRule1 i v vs di em = return di2
   where  dvs = filter (isTypeMismatch v) vs
          dis = (toValue i : dvs)
          di1 = case (isTransValue v, null dvs) of 
                  (True, _) -> addToDInfo 1 [toValue i, v] di
                  (False,False) -> addToDInfo 1 dis di
                  _  -> di
          di2 = if (isMissCases em i) then addToDInfo 11 [toValue i] di1 else di1

chkOtherOldRules :: Module -> SummaryType
chkOtherOldRules m = di2
--  m <- getModuleFromFile fileName
  where
     fs = moduleDefinedFunctions m
     blks = concatMap functionBody fs
     insts = concatMap basicBlockInstructions blks
     dris = filter isDivRemInst insts
     sdris = filter isStoreInst insts    -- mapMaybe succInst dris
     check1 = isConstantZero . binaryRhs
     check2 = isConstantUndef . storeValue
     vs1 = map toValue $ filter check1 dris ++ filter check2 sdris
     di = IM.insert 3 (HS.fromList vs1) mempty     
     check3 f = case functionExitInstruction f of
        Just RetInst {retInstValue = Just rv} ->  isClass rv  -- && isConstantType fType
        Just i@RetInst {retInstValue = Nothing} ->  isClass (toValue i) -- && isConstantType fType
--        Just UnreachableInst {} -> True
--        Just ResumeInst {} -> True
        _ -> False
       where  fType = simpleType (functionReturnType f)
              vType = simpleType . valueType . memAccessBase 
              isClass v = if isStructType (vType v) then isPrefixOf "class." $ structTypeName (vType v)
                          else False
     fvs = map toValue $ filter check3 fs
     di2 = IM.insertWith HS.union 13 (HS.fromList fvs) di

--------------
getCustodial :: Analysis SummaryType 
getCustodial = do
  s <- analysisGet 
  let cm = custodialCache s
      di = IM.insert 5 (HS.fromList. concat $ HM.elems cm) mempty
  return di


cacheCustodial i ptr sv ics = do
  s <- analysisGet 
  let ptr' = memAccessBase ptr
      sv' = memAccessBase sv
      cm = custodialCache s
      ptrCu = HM.lookupDefault [] ptr' cm 
      svCu = HM.lookupDefault [] sv' cm 
      isAlias = mayAlias ics ptr' sv'
  case (isPointerValue ptr', null ptrCu, null svCu) of
    (True, True, True) -> analysisPut s { custodialCache = HM.insert ptr' [toValue i,ptr'] cm }
    (True, True, False) -> analysisPut s { custodialCache = HM.delete sv' cm }
    (True, False, False) -> 
         if isAlias then analysisPut s { custodialCache = HM.delete sv' $ HM.delete ptr' cm }
         else analysisPut s { custodialCache = HM.delete sv' cm }
    (True, False, True) -> analysisPut s { custodialCache = HM.insertWith (++) ptr' [toValue i] cm }
    _  -> return ()


cachePhiValue i ivs = 
  case phi2Var i of 
    Just v  -> if null phiInts then return () 
               else cacheIntValue v (head phiInts)
    Nothing -> return ()
  where 
     phiInts = filter isConstantInt ivs

cacheIntValue ptr sv = do
   s <- analysisGet
   let vPtr = memAccessBase ptr
   vInt <- getIntValue vPtr
   analysisPut s { intValueCache = HM.insert vPtr vInt (intValueCache s) }

updateIntValue i op lv rv = do   
   s <- analysisGet
   lv' <- getIntValue lv
   rv' <- getIntValue rv
   case (lv', rv') of 
     (Just lvi, Just rvi) -> analysisPut s {intValueCache = 
                HM.insert (memAccessBase i) (op lvi rvi) (intValueCache s)}
     _ -> return ()

getIntValue v = do 
   s <- analysisGet   
   case HM.lookup v (intValueCache s) of 
     Just iv -> return iv
     Nothing -> return (fromConstantInt v)

isMissCases m i =
  case (isMissDefault i, getEnumValues m i) of 
    (True, Just evals) -> length evals /= length (switchCases i)
--    (False, Nothing) -> True
    _  -> False

isMissDefault v = 
  case getValueBody v of 
   Just s -> not (isInfixOf "default:" s) && not (isInfixOf "default " s)
   Nothing -> False

isTypeMismatch v1 v2 =  case1 || case2
  where 
   case1 = v1ty /= v2ty
   case2 = isEnumVar v1  && isIntType v2ty 
   v1ty = simpleType (getAccessType v1)
   v2ty = simpleValueType v2


-----------------------------------------------------------------------------
--- 
resolveInitializer :: Value -> [Value] -> Maybe Value
resolveInitializer v [] = return (stripBitcasts v)
resolveInitializer v (ix:ixs) = do
  intVal <- fromConstantInt ix
  case valueContent v of
    ConstantC ConstantArray { constantArrayValues = vs } ->
      if length vs <= intVal then Nothing else resolveInitializer (vs !! intVal) ixs
    ConstantC ConstantStruct { constantStructValues = vs } ->
      if length vs <= intVal then Nothing else resolveInitializer (vs !! intVal) ixs
    _ -> Nothing

   
--
isVirtualThunk :: Function -> Bool
isVirtualThunk f = case funDName f of
   Left _ -> False
   Right sname ->  case sname of
      ABI.OverrideThunk _ _ -> True
      ABI.OverrideThunkCovariant _ _ _ -> True
      _ -> False    
  
    
-- | Get the value called by a Call or Invoke instruction
calledValue :: Instruction -> Value
calledValue CallInst { callFunction = v } = v
calledValue InvokeInst { invokeFunction = v } = v
calledValue i = error ("Expected Call or Invoke instruction: " ++ show i)

-- | Return True if the given call (or invoke) instruction is a call
-- to a statically known function (rather than a function pointer).
isDirectCall :: Instruction -> Bool
isDirectCall ci = isDirectCall' cv
  where
    cv = calledValue ci
    isDirectCall' :: Value -> Bool
    isDirectCall' v = case valueContent v of
      FunctionC _ -> True
      ExternalFunctionC _ -> True
      GlobalAliasC GlobalAlias { globalAliasTarget = t } -> isDirectCall' t
      InstructionC BitcastInst { castedValue = c } -> isDirectCall' c
      _ -> False 


-- | Determine if the given function is a C2 constructor or not.  C1
-- and C3 don't give us the information we want, so ignore them
isC2Constructor :: Function -> Bool
isC2Constructor f = case funDName f of
    Left _ -> False
    Right structuredName -> case universeBi structuredName of
        [ABI.C2] -> True
        _ -> False
    
isConstructor :: Function -> Bool
isConstructor f = case funDName f of
   Left _ -> False
   Right structuredName -> case universeBi structuredName of
      [ABI.C2] -> True
      [ABI.C1] -> True
      [ABI.C3] -> True
      _ -> False

-- | Strip a prefix, operating as the identity if the input string did
-- not have the prefix.
stripPrefix' :: String -> String -> String
stripPrefix' pfx s = fromMaybe s (stripPrefix pfx s)

stripNamePrefix :: String -> String
stripNamePrefix =
  stripPrefix' "struct." . stripPrefix' "class."

typeToName :: Type -> ABI.Name
typeToName (TypeStruct (Right n) _ _) =
  case parseTypeName (stripNamePrefix (T.unpack n)) of
    Right tn -> tn
    Left e -> error ("LLVM.Analysis.ClassHierarchy.typeToName: " ++ e)
typeToName t = error ("LLVM.Analysis.ClassHierarchy.typeToName: Expected named struct type: " ++ show t)

nameToString :: ABI.Name -> String
nameToString n = fromMaybe errMsg (unparseTypeName n)
  where
    errMsg = error ("Could not encode name as string: " ++ show n)

funDName :: Function -> Either String ABI.DecodedName
funDName = ABI.demangleName . identifierAsString . functionName

   
------------------
localDestNotFinalized :: AccessPath -> Function -> Bool
localDestNotFinalized acp f = fromMaybe False $ do
  AllocaInst {} <- fromValue (accessPathBaseValue acp)
  return True

isFinalizerContext :: (HasFunction funcLike)
                      => CallGraph
                      -> FinalizerSummary
                      -> funcLike
                      -> Analysis Bool
isFinalizerContext cg finSumm flike =
  liftM or $ mapM isFinalizer callers
  where
    f = getFunction flike
    callers = allFunctionCallers cg f
    isFinalizer callee =
      liftM2 fromMaybe (return False) $ runMaybeT (checkFinCtx callee)
    checkFinCtx callee = do
      nargs <- formalArgumentCount callee
      liftM or $ mapM (isFinalizerArg callee) [0..nargs]
    isFinalizerArg callee ix =
      liftM (elem PAFinalize) $ lift $ lookupArgumentSummaryList finSumm callee ix

formalArgumentCount :: Value -> MaybeT Analysis Int
formalArgumentCount v =
  case valueContent' v of
    FunctionC fn -> return $ length $ functionParameters fn
    ExternalFunctionC ef -> return $ length $ externalFunctionParameterTypes ef
    _ -> fail "Not a function"



calleeFormalAt :: (IsValue v) => v -> Int -> Maybe Argument
calleeFormalAt target ix = do
  callee <- fromValue (toValue target)
  let params = functionParameters callee
  params `at` ix

accessPathBaseArgument :: AccessPath -> Maybe Argument
accessPathBaseArgument p =
  fromValue $ valueContent' (accessPathBaseValue p)

-- The end type does not always match up for various unimportant
-- reasons.  All that really matters is that the base types and path
-- components match.  It might be worth pushing this down to the
-- AccessPath Eq instance, but I don't know what effect that will have
-- on the derived Ord instance.
equivAccessPaths :: AbstractAccessPath -> AbstractAccessPath -> Bool
equivAccessPaths (AbstractAccessPath bt1 _ c1) (AbstractAccessPath bt2 _ c2) =
  bt1 == bt2 && c1' == c2'
  where
    c1' = filter (/=AccessDeref) $ map snd c1
    c2' = filter (/=AccessDeref) $ map snd c2

-- Testing


defectSummaryToTestFormat :: DefectSummary -> Map String (Set String)
defectSummaryToTestFormat (DefectSummary s _) = undefined
--  F.foldr convert mempty s
--  where
--    convert a m =
--      let f = argumentFunction a
--          k = show (functionName f)
--          v = show (argumentName a)
--      in M.insertWith S.union k (S.singleton v) m

--------------
