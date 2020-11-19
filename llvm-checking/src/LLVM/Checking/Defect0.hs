{-# LANGUAGE DeriveGeneric, TemplateHaskell, ViewPatterns, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, CPP #-}
-- | An analysis to identify program defect from some rules.
--

module LLVM.Checking.Defect (
  DefectSummary(..),
  identifyDefects,
  checkOtherRules,
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

import LLVM.Slicing 
import LLVM.Slicing.Static.Symbolic.SymSlicer0 as Sym
import LLVM.Slicing.Static.InfoFlow.InfoFlowSlicer as IF 

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
                     icsSumm :: IndirectCallSummary
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
                          , freeCache :: Set Value }

data DefectInfo = DI {
       _defectRuleSumm :: SummaryType        --- IntMap (HashSet Value)
     }  deriving (Eq,Ord,Show,Generic)

$(makeLenses ''DefectInfo)

type Analysis = AnalysisMonad DefectEnv DefectState


identifyDefects :: (FuncLike funcLike, HasFunction funcLike, HasCFG funcLike,
                     HasCDG funcLike, HasDomTree funcLike)
                    =>  -- Module ->    -- for fn2mod
                    Module -> DependencySummary
                    -> Lens' compositeSummary DefectSummary
                    -> Getter compositeSummary ReturnSummary
                    -> ComposableAnalysis compositeSummary funcLike
identifyDefects m ds lns depLens =
  composableDependencyAnalysisM runner (defectAnalysis m) lns depLens
  where
    runner a = runAnalysis a ds constData cache
    constData = DE ics uses cha enums undefined
    cache = DState mempty mempty mempty
    enums = genCEnumMap m
    uses = computeUsesOf m
    ics = identifyIndirectCallTargets m 
    cha = runCHA m

defectAnalysis :: (FuncLike funcLike, HasFunction funcLike, HasCFG funcLike,
                     HasCDG funcLike, HasDomTree funcLike)
                     => Module
                     -> ReturnSummary
                     -> funcLike
                     -> DefectSummary
                     -> Analysis DefectSummary
defectAnalysis m retSumm funcLike s = do    
  let envMod e = e {  nullVals = funcNullPtrs m f
                   }
      analysis = fwdDataflowAnalysis top meet (transfer s retSumm)
  localInfo <- analysisLocal envMod (dataflow funcLike analysis top)
  custodial <- getCustodial
  let DI di = dataflowResult localInfo
      di2 = IM.unionWith HS.union di custodial
  return $! (defectSummary %~ IM.unionWith HS.union di2 ) s
  where
    f = getFunction funcLike  

top :: DefectInfo
top = DI mempty 

meet :: DefectInfo -> DefectInfo -> DefectInfo
meet (DI d1) (DI d2) = DI $ IM.unionWith HS.union d1 d2


addToDInfo :: Int -> [Value] -> DefectInfo -> DefectInfo
addToDInfo n ds (DI dis) = DI $ IM.insertWith HS.union n (HS.fromList ds) dis

addOneToSumm n d (DI dis) = DI $ IM.insertWith HS.union n (HS.singleton d) dis


transfer :: DefectSummary
         -> ReturnSummary
         -> DefectInfo
         -> Instruction
         -> Analysis DefectInfo
transfer ds outSumm di i =  do  
  em <- analysisEnvironment enumMap
  nvs <- analysisEnvironment nullVals
  ics <- analysisEnvironment icsSumm
  uses <- analysisEnvironment useSumm
  cha <- analysisEnvironment chaSumm
  case i of 
    SwitchInst { switchValue = v, switchDefaultTarget = dt, switchCases = (map fst -> vs) } -> 
       checkRule1 i v vs di em
       
    LoadInst {loadAddress = la} ->  checkRule4 i la di
         
    StoreInst {storeAddress = ptr, storeValue = sv} -> do
--       cacheIntValue ptr sv
       cacheCustodial i ptr sv ics  
       di2 <- checkRule2 i sv di nvs
       di3 <- checkRule3 i sv di2
       checkRule4 i ptr di3 
    AtomicRMWInst {atomicRMWPointer = ptr, atomicRMWValue = av} -> do
--       cacheIntValue ptr av
       cacheCustodial i ptr av ics  
       di2 <- checkRule2 i av di nvs
       checkRule4 i ptr di2 
    AtomicCmpXchgInst {atomicCmpXchgPointer = ptr, atomicCmpXchgNewValue = nv} -> do
--       cacheIntValue ptr nv
       cacheCustodial i ptr nv ics  
       di2 <- checkRule2 i nv di nvs
       checkRule4 i ptr di2       
    InsertValueInst {insertValueAggregate = a, insertValueValue = iv} -> do
--       cacheIntValue a iv
       cacheCustodial i a iv ics  
       di2 <- checkRule2 i iv di nvs
       checkRule4 i a di2 
    PhiNode {phiIncomingValues = (map fst -> ivs)} -> do
--       cachePhiValue i ivs
       checkRule2 i (toValue i) di nvs
    
    GetElementPtrInst { getElementPtrValue = base, getElementPtrIndices = ixs } -> do
       di2 <- checkRule4 i (toValue i) di   
       checkRule6 i base ixs di2 
      
    CallInst { callFunction = fv, callArguments = (map fst -> avs) } -> do
       di2 <- checkRule2' i avs di nvs   
       di3 <- checkRule4' i fv avs di2 
       checkRule7 i fv avs di3
        
    InvokeInst { invokeFunction = fv, invokeArguments = (map fst -> avs) } -> do
       di2 <- checkRule2' i avs di nvs 
       di3 <- checkRule4' i fv avs di2
       checkRule7 i fv avs di3
       
    DivInst { binaryLhs = l, binaryRhs = r }  -> 
       checkRule3 i r di
    RemInst { binaryLhs = l, binaryRhs = r }  -> 
       checkRule3 i r di
    
    ICmpInst {cmpV1 = v1, cmpV2 = v2} ->
       checkRule9 i v1 v2 di
    FCmpInst {cmpV1 = v1, cmpV2 = v2} ->
       checkRule9 i v1 v2 di
    
    BitcastInst {castedValue = cv} -> 
       checkRule15 i cv di
       
    
--    RetInst {retInstValue = Just rv} ->
--       checkRule
    
    ResumeInst {resumeException = ev} ->  checkRule8 i di    
    UnreachableInst {} ->  checkRule8 i di
    
          
           
    _  -> return di


   
--------
checkRule15 i cv di = 
  if isStructType ity then return di2 else return di
  where  
      ity = simpleType $ valueType i
      isDerived = structTypeName ity == "class.Derived"
      di2 = addToDInfo 15 [toValue i, cv] di
    

checkRule9 i v1 v2 di = 
  case (isUnsignedVar v1, isUnsignedVar v2) of 
     (True, False) -> return $ addToDInfo 9 [toValue i,v1] di
     (False, True) -> return $ addToDInfo 9 [toValue i,v2] di
     _ -> return di
     

checkRule8 i di = do
 case instructionFunction i of
      Just f -> return $ addToDInfo 8 [toValue f] di
      Nothing -> return di 

checkRule7 i fv avs di = do
  s <- analysisGet 
  let vs = S.fromList $ map memAccessBase avs
  case ( isFree fv, S.null $ S.intersection vs (freeCache s) ) of
    (True, False) -> return $ addToDInfo 7 [toValue i] di
    (True, True) -> analysisPut s { freeCache = S.union vs (freeCache s) } >> return di
    _  -> return di
    

checkRule6 i base ixs di = 
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


checkRule4' i fv args di = 
  if isStrcpy fv then checkRule4 i (args!!1) di else return di 


checkRule4 i ptr di = 
   case valueContent ptr of 
     InstructionC GetElementPtrInst { getElementPtrValue = base, getElementPtrIndices = ixs } ->
         if isIxOut base ixs then return di2 else return di
     _ -> return di    
   where di2 = addToDInfo 4 [toValue i, ptr] di

isIxOut base ixs@(ix:xs) = 
  case (valueContent base, fromConstantInt ix) of
    (ConstantC ConstantArray { constantArrayValues = vs }, Just k) -> length vs <= k 
    (ConstantC ConstantStruct { constantStructValues = vs }, Just k) -> length vs <= k 
    (v, Just k) -> typeSize (valueType base) <= k
    _ ->  case (valueType base, ixs) of
      (TypePointer (TypeArray _ _) _, (valueContent' -> ConstantC ConstantInt {constantIntValue = 0 }):_) ->  False
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


checkRule3 i r di = 
   if isConstantZero r || isConstantUndef r then return di2 else return di
  where  di2 = addToDInfo 2 (toValue i : directValueRefs r) di
   

checkRule2 i v di nullVs = return di2
   where di1 = addToDInfo 2 (toValue i : directValueRefs v) di
         di2 = if isInjectsNull v || S.member v nullVs then  di1 else di
         
checkRule2' i vs di nullVs = return di2
   where vs' = filter (flip S.member nullVs) vs
         di1 = addToDInfo 2 (toValue i : vs') di
         di2 = if null vs' then di else di1 

checkRule1 i v vs di em = return di2
   where  dvs = filter (isTypeMismatch v) vs
          dis = (toValue i : dvs)
          di1 = case (isTransValue v, null dvs) of 
                  (True, _) -> addToDInfo 1 [toValue i, v] di
                  (False,False) -> addToDInfo 1 dis di
                  _  -> di
          di2 = if (isMissCases em i) then addToDInfo 11 [toValue i] di1 else di1

-- rules3
checkOtherRules :: Module -> SummaryType
checkOtherRules m = di2
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
-- rule5
getCustodial :: Analysis SummaryType 
getCustodial = do
  s <- analysisGet 
  let cm = custodialCache s
      di = IM.insert 5 (HS.fromList. concat $ HM.elems cm) mempty
  return di


     
----------------------------------------------------------------
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
     (Just lvi, Just rvi) -> analysisPut s {intValueCache = HM.insert (memAccessBase i) (op lvi rvi) (intValueCache s)}
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


getEnumValues m v = 
  case null venums of 
    True -> Nothing
    False -> Just enumVals
  where  
     venums = getEnumDef v
     m' = M.filterWithKey filtF m
     filtF k _ = isInfixOf (" " ++ k ++ " ") (head venums) 
     enumVals = head (M.elems m')


isTypeMismatch v1 v2 =  case1 || case2
  where 
   case1 = v1ty /= v2ty
   case2 = isEnumVar v1  && isIntType v2ty 
   v1ty = simpleType (getAccessType v1)
   v2ty = simpleValueType v2


getAccessType v = fromMaybe (valueType $ memAccessBase v) (getAccessType' v)

getAccessType' :: Value -> Maybe Type
getAccessType' v = do
  i <- fromValue v
  ap <- accessPath i
  let vty = accessPathEndType ap 
  return vty

valueAsAccessPath :: Value -> Maybe AccessPath
valueAsAccessPath v = fromValue v >>= accessPath

simpleValueType :: IsValue v => v -> Type
simpleValueType = simpleType . valueType . memAccessBase . toValue

simpleType ty = 
  case ty of 
    TypePointer ty' _ -> simpleType ty'
    TypeArray _ ty' -> simpleType ty'
    TypeFunction ty' _ _ -> simpleType ty'
    TypeVector _ ty' -> simpleType ty'
    _   -> ty

typeSize ty = 
  case simpleType ty of 
    TypePointer _ n -> n
    TypeArray n _ -> n
    TypeVector n _ -> n
    _   -> 0

fromConstantInt :: Value -> Maybe Int
fromConstantInt v =
  case valueContent v of
    ConstantC ConstantInt { constantIntValue = iv } ->
      return $ fromIntegral iv
    _ -> Nothing
 


---
genCEnumMap :: Module -> Map String [(String,Int)]
genCEnumMap m = M.fromList $ zip enames evals
  where
    cenums = moduleInterfaceEnumerations m
    enames = map enumName cenums
    evals = map enumValues cenums




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


-----
{-
mustExecuteValue :: Value -> Analysis (Maybe Value)
mustExecuteValue v =
  case valueContent' v of
    InstructionC SelectInst {} -> return Nothing
    InstructionC i@PhiNode { phiIncomingValues = ivs } -> do
      s <- analysisGet
      case HM.lookup i (phiCache s) of
        Just mv -> return mv
        Nothing -> do
          mv <- mustExec' i ivs
          analysisPut s { phiCache = HM.insert i mv (phiCache s) }
          return mv
    _ -> return (Just v)

mustExec' :: Instruction -> [(Value, Value)] -> Analysis (Maybe Value)
mustExec' i ivs = do
  cdg <- analysisEnvironment controlDepGraph
  dt <- analysisEnvironment domTree
  let cdeps = directControlDependencies cdg i
  case cdeps of
    [] -> return Nothing
    [_] -> do
      let predTerms = map (id *** (basicBlockTerminatorInstruction . toBB)) ivs
          nonBackedges = filter (isNotBackedge dt i) predTerms
      case filter (isUnconditional . snd) nonBackedges of
        [] -> return Nothing
        [(v,_)] -> return (Just v)
        _ -> return Nothing
    _ -> return Nothing
  where
    toBB v =
      case valueContent v of
        BasicBlockC bb -> bb
        _ -> error ("LLVM.Checking.Defect.mustExec': Expected basic block: " ++ show v)
    isUnconditional UnconditionalBranchInst {} = True
    isUnconditional _ = False
    isNotBackedge g inst (_, br) = not (dominates g inst br)
-}


isInjectsNull :: Value -> Bool
isInjectsNull v = isPointerType v && case valueContent v of
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

isNullInitialized gv = 
   case globalVariableInitializer gv of
      Nothing -> True -- We'll treat uninintialized as null
      Just i -> case valueContent i of
        ConstantC (UndefValue {}) -> True
        ConstantC (ConstantPointerNull {}) -> True
        _ -> False




--- Checking the index out of range of array or struct
-- | Return the innermost type and the index into that type accessed
-- by the GEP instruction with the given base and indices.
fieldDescriptor :: Value -> [Value] -> Maybe (Type, Int)
fieldDescriptor base ixs =
  case (valueType base, ixs) of
    -- A pointer being accessed as an array
    (_, [_]) -> Nothing
    -- An actual array type (first index should be zero here)
    (TypePointer (TypeArray _ _) _, (valueContent' -> ConstantC ConstantInt { constantIntValue = 0 }):_) ->
      Nothing
    -- It doesn't matter what the first index is; even if it isn't
    -- zero (as in it *is* an array access), we only care about the
    -- ultimate field access and not the array.  Raw arrays are taken
    -- care of above.
    (TypePointer t _, _:rest) -> return $ walkType t rest
    _ -> Nothing

walkType :: Type -> [Value] -> (Type, Int)
walkType t [] = error ("LLVM.Analysis.PointsTo.Andersen.walkType: expected non-empty index list for " ++ show t)
walkType t [(valueContent -> ConstantC ConstantInt { constantIntValue = iv })] =
  (t, fromIntegral iv)
walkType t (ix:ixs) =
  case t of
    -- We can ignore inner array indices since we only care about the
    -- terminal struct index.  Note that if there are no further
    -- struct types (e.g., this is an array member of a struct), we
    -- need to return the index of the array... FIXME
    TypeArray _ t' -> walkType t' ixs
    TypeStruct _ ts _ ->
      case valueContent ix of
        ConstantC ConstantInt { constantIntValue = (fromIntegral -> iv) } ->
          case iv < length ts of
            True -> walkType (ts !! iv) ixs
            False -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: index out of range " ++ show iv ++ " in " ++ show t)
        _ -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: non-constant index " ++ show ix ++ " in " ++ show t)
    _ -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: unexpected type " ++ show ix ++ " in " ++ show t)


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

    
------------------
{-}
identifyTransferredArguments :: (HasFunction funcLike)
                                => IndirectCallSummary
                                -> SAPSummary
                                -> Set AbstractAccessPath
                                -> DefectSummary
                                -> funcLike
                                -> Analysis DefectSummary
identifyTransferredArguments pta sapSumm ownedFields trSumm flike = do
  -- The preliminary pass with checkWrittenFormals uses the results of
  -- the SymbolicAccessPath analysis to resolve complex
  -- interprocedural field writes.  This includes container-like
  -- manipulations.
  trSumm' <- foldM checkWrittenFormals trSumm formals
  foldM checkTransfer trSumm' (functionInstructions f)
  where
    f = getFunction flike
    formals = functionParameters f
    checkWrittenFormals s formal =
      return $ fromMaybe s $ do
        wps <- writePaths formal sapSumm
        return $ foldr (checkWrittenFormal formal) s wps
    checkWrittenFormal formal (_, p) s =
      fromMaybe s $ do
        _ <- F.find (equivAccessPaths p) ownedFields
        return $ (transferArguments %~ S.insert formal) s

    checkTransfer s i =
      case i of
        -- This case handles simple transfers (where a function
        -- locally stores an argument into a field of another
        -- argument.  More complex cases are handled in
        -- checkWrittenFormals
        StoreInst { storeValue = (valueContent' -> ArgumentC sv) }
          | sv `elem` formals -> return $ fromMaybe s $ do
            -- We don't extract the storeAddress above because the
            -- access path construction handles that
            acp <- accessPath i
            let absPath = abstractAccessPath acp
            -- If the store address is a local (that is never
            -- finalized), then this is obviously not transferred.
--            return () `debug` (" Checking a transfer in " ++ show (functionName f) ++ " " ++ show absPath ++ " from " ++ show acp)
            case S.member absPath ownedFields && not (localDestNotFinalized acp f) of
              True -> return $! (transferArguments %~ S.insert sv) s
              False -> return s
          | otherwise -> return s

        CallInst { callFunction = callee, callArguments = (map fst -> args) } ->
          calleeArgumentFold (argumentTransfer args) s pta callee args
        InvokeInst { invokeFunction = callee, invokeArguments = (map fst -> args) } ->
          calleeArgumentFold (argumentTransfer args) s pta callee args
        _ -> return s

    -- We only care about call arguments that are actually Arguments
    -- in the caller because field->field transfers are outside the
    -- scope of the analysis.
    argumentTransfer actuals callee s (ix, (valueContent' -> ArgumentC arg)) = do
      annots <- lookupArgumentSummaryList s callee ix
      case PATransfer `elem` annots of
        False ->
          -- In this case, the argument position isn't known to be a
          -- transfer argument.  We still have to check to see if the
          -- argument is written into a field of another argument that
          -- /is/ owned.
          --
          -- For each of the write paths we get here, find the index
          -- of the relevant target argument and look that up in
          -- @args@.  Extend the path to args if necessary and see if
          -- the result is owned.
          return $ fromMaybe s $ do
            wps <- writePaths arg sapSumm
            return $ foldr (checkWritePath actuals arg) s wps
        True -> return $ (transferArguments %~ S.insert arg) s
    argumentTransfer _ _ s _ = return s

    checkWritePath actuals writtenFormal (destArg, wp) s =
      fromMaybe s $ do
        let destIx = argumentIndex destArg
        actualDst <- actuals `at` destIx
        extension <- accessPathOrArgument actualDst
        case extension of
          Left _ -> do
            _ <- F.find (equivAccessPaths wp) ownedFields
            return $ (transferArguments %~ S.insert writtenFormal) s
          Right cap -> do
            -- Ensure the base of the access path is an Argument
            _ <- accessPathBaseArgument cap
            let absPath = abstractAccessPath cap
            extendedPath <- absPath `appendAccessPath` wp
            _ <- F.find (equivAccessPaths extendedPath) ownedFields
            return $ (transferArguments %~ S.insert writtenFormal) s
-}

localDestNotFinalized :: AccessPath -> Function -> Bool
localDestNotFinalized acp f = fromMaybe False $ do
  -- An alloca means we stored into a local.
  AllocaInst {} <- fromValue (accessPathBaseValue acp)
  -- FIXME: Check to ensure it really isn't finalized.  For now we are
  -- assuming that locals are never finalized, which is almost certainly
  -- true by definition...
  return True

-- | Determine whether or not the given function is a finalizer.  We
-- need this because we only want to infer ownership from finalizer
-- calls *within another finalizer*.
--
-- This lets us ignore almost all "local" deletes where some
-- locally-allocated value is stored in a struct and then finalized.
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

-- | Add any field passed to a known finalizer to the accumulated Set.
--
-- This will eventually need to incorporate shape analysis results.
-- It will also need to distinguish somehow between fields that are
-- finalized and elements of container fields that are finalized.
identifyOwnedFields :: (HasFunction funcLike)
                       => CallGraph
                       -> IndirectCallSummary
                       -> FinalizerSummary
                       -> SAPSummary
                       -> Set AbstractAccessPath
                       -> funcLike
                       -> Analysis (Set AbstractAccessPath)
identifyOwnedFields cg pta finSumm sapSumm ownedFields funcLike = do
  isFin <- isFinalizerContext cg finSumm funcLike
  case isFin of
    True -> foldM checkFinalize ownedFields insts
    False -> return ownedFields
  where
    insts = functionInstructions (getFunction funcLike)
    checkFinalize acc i =
      case i of
        CallInst { callFunction = cf, callArguments = (map fst -> args) } ->
          calleeArgumentFold addFieldIfFinalized acc pta cf args
        InvokeInst { invokeFunction = cf, invokeArguments = (map fst -> args) } ->
          calleeArgumentFold addFieldIfFinalized acc pta cf args
        _ -> return acc

    addFieldIfFinalized target acc (ix, arg) = do
      annots <- lookupArgumentSummaryList finSumm target ix
      case PAFinalize `elem` annots of
        -- In this case we aren't seeing a known finalizer call;
        -- instead, check to see if the call is known to finalize some
        -- field of one of its arguments.
        False -> return $ fromMaybe acc $ do
          calleeArg <- calleeFormalAt target ix
          ffs <- finalizedPaths calleeArg sapSumm
          case valueContent' arg of
            ArgumentC _ ->
              -- All of the paths described by ffs are owned fields,
              -- so union them with acc.  We don't need the ArgumentC
              -- binding here; it is only to let us know that this
              -- actual is really a formal in the current function and
              -- we need to propagate information about it upwards in
              -- the summary.
              return $ acc `S.union` S.fromList ffs
            InstructionC i -> do
              -- We don't need the base argument, we just need to know
              -- that the base is an Argument.
              (cap, _) <- anyArgumentAccessPath i
              let absPath = abstractAccessPath cap
                  extended = mapMaybe (appendAccessPath absPath) ffs
              -- Extend absPath by each of the paths described in ffs.
              -- These are *all* owned fields:
              return $ acc `S.union` S.fromList extended
            _ -> Nothing
        -- Calling a known finalizer on some value
        True ->
          case valueContent' arg of
            -- Calling a finalizer on the result of a function call.
            -- Here, we need to look up the function summary of cf and
            -- see if it is returning some access path of one of its
            -- actual arguments.
            InstructionC CallInst { callFunction = (valueContent' -> FunctionC cf)
                                  , callArguments = (map fst -> args)
                                  } ->
              return $ fromMaybe acc $ do
                calleeArg <- calleeFormalAt cf ix
                rps <- returnedPaths cf calleeArg sapSumm
                -- These paths (from @calleeArg@) are returned.  We
                -- have to extend each path with the actual argument
                -- passed in that position.
                actualAtIx <- args `at` argumentIndex calleeArg
                pathOrArg <- accessPathOrArgument actualAtIx
                case pathOrArg of
                  Left _ ->
                    return $ acc `S.union` S.fromList rps
                  Right p ->
                    let absPath = abstractAccessPath p
                        rps' = mapMaybe (appendAccessPath absPath) rps
                    in return $ acc `S.union` S.fromList rps'
            -- Calling a finalizer on a local access path
            InstructionC i -> return $ fromMaybe acc $ do
              accPath <- accessPath i
              let absPath = abstractAccessPath accPath
              return $ S.insert absPath acc
            _ -> return acc

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
