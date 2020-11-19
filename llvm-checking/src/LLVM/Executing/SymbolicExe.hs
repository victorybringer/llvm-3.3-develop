------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-- {-# OPTIONS_GHC -DZYZ_DEBUG #-}

module LLVM.Executing.SymbolicExe (
  identifySymExe, valueToSbv,
  mkValSbv, mkValSbv_, mkExTypeSbv,
  --
  getAggValues, getGepFinalVal, 
  getPtrValue, isLocalStoreAddr
  ) where

import           Control.Lens  hiding  ( at, (.>) ) 
import           Control.Arrow ( (***),(&&&) )
import           Control.Monad ( foldM, liftM, when )
import           Control.Monad.Trans ( lift )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Failure as F ( Failure, failure )
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import           Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import           Data.Set ( Set )
import qualified Data.Set as S
import           Data.IntSet ( IntSet )
import           Data.Map ( Map )
import qualified Data.Map.Strict as M
import qualified Data.Text as T  ( unpack )
import           Data.Maybe
import           Data.List  ( foldl', find, intersect )
import qualified Data.Foldable as F 
import           Safe.Failure  ( at )
import           Text.Printf  ( printf )
import           Debug.Trace  ( trace )


--
import           Data.SBV 
import           Data.SBV.Internals ( mkSymSBV, SVal(..), unSBV )
import           Data.SBV.Tools.Overflow

import           LLVM.Analysis
import           LLVM.Analysis.CFG
import           LLVM.Analysis.CDG
import           LLVM.Analysis.CallGraph
import           LLVM.Analysis.CallGraphSCCTraversal
import           LLVM.Analysis.Dataflow
import           LLVM.Analysis.Dominance
import           LLVM.Analysis.BlockReturnValue
import           LLVM.Analysis.PointsTo
import           LLVM.Analysis.PointsTo.TrivialFunction
import           LLVM.Analysis.AccessPath

import           LLVM.Checking.Inference.SAP  
import           LLVM.Checking.Inference.SAPPTRel 
import           LLVM.Checking.Inference.IndirectCallResolver 
import           LLVM.Checking.Inference.Escape  
import           LLVM.Checking.Inference.Output 
-- import           LLVM.Checking.CheckSummary  as ChkSumm
import           Foreign.Inference.Interface

import           LLVM.Slicing.Util.Utils
import           LLVM.Executing.SymExeType



symExeTransfer :: SymExeInfo -> Instruction -> SymExecutor SymExeInfo
symExeTransfer si i =  do   -- dbgIt i $  
#ifdef ZYZ_DEBUG
  liftIO. putStrLn $ printf ("(%s)%s \"%s\" ")(show $ valueUniqueId i)
                      (show $ toSrcLnStr' [toValue i]) (show i)
#endif
  case i of    
    StoreInst {storeAddress = ptr, storeValue = sv} -> 
      updSymExeInfo i (ptr,sv) si
    AtomicRMWInst {atomicRMWPointer = ptr, atomicRMWValue = av} ->
      updSymExeInfo i (ptr,av) si

    AtomicCmpXchgInst {atomicCmpXchgPointer = ptr, atomicCmpXchgNewValue = nv
                      , atomicCmpXchgComparison = c } -> do
      (cv,si') <- valueToSbv c si
      let pv = getPtrValue ptr
          eq mv = isSBVTrue $ anyEqSExpr mv cv
      case lkpValStr pv si of
        Just mv  -> if eq mv 
                    then updSymExeInfo i (ptr,nv) si'
                    else return si
        _  ->  updSymExeInfo i (ptr,nv) si

        
    CallInst { callFunction = fv, callArguments = avs } ->
      callTransfer i fv (map fst avs) si
    InvokeInst { invokeFunction = fv, invokeArguments = avs } ->
      callTransfer i fv (map fst avs) si

    _  -> snd <$> valueToSbv (toValue i) si
{-# INLINE symExeTransfer #-}

updSymExeInfo :: Instruction -> (Value,Value) -> SymExeInfo -> SymExecutor SymExeInfo
updSymExeInfo i (ptr,v) si = do
  (sbv,si') <- valueToSbv v si
  initM <- symExeEnvironment argSbvMap
  utbl <- symExeEnvironment unsignsTbl
  si2 <- case valueContent ptr of 
      InstructionC PhiNode {phiIncomingValues = (map fst -> ivs)} -> do
        let ptNames = mapMaybe toVarName (map memAccessBase ivs)  
            sbvs = take (length ptNames) (repeat sbv)
            !si2 = if null ptNames then si
                  else xtdVars ptNames sbvs si
        return si2
      InstructionC  SelectInst { selectCondition = c,
          selectTrueValue = t, selectFalseValue = f } -> do
        (cv,si2) <- valueToSbv c si'  
        let ptr' = if chkV then t else f 
            chkV = toBool cv
            pv = getPtrValue ptr'
            si3 = updValStr pv sbv si2
        case IM.lookup (valueUniqueId pv) initM of
          Just argSbv  ->  lift . softConstrain $ argSbv .== sbv
          Nothing  -> return ()
        return si3   
      _ -> do
        let pv = getPtrValue ptr
            !si2 = if isAggregate ptr || isAggType pv 
                  then xtdVar (toVarName' pv) sbv si'
                  else updValStr pv sbv si'   
        case IM.lookup (valueUniqueId pv) initM of
          Just argSbv  ->  lift . softConstrain $ argSbv .== sbv
          Nothing  -> return ()
        return si2 
  let isOverflow = fromSBool $ chkValOverflow utbl (ptrUnsign,ptr,sbv)
      iCond = getCurrCond i si
      ptrUnsign = isUnsign ptr 
      isUnsign = or . map (chkVarTypeWith utbl) . refs
      ofSbv = ite iCond isOverflow nilSExpr   -- (intToSExpr 0 )
      si3 = if ptrUnsign
            then updVar ("!_#8_" ++ toVarName' i) ofSbv si2
            else si2
  return si3
      

callTransfer :: Instruction -> Value -> [Value] -> SymExeInfo -> SymExecutor SymExeInfo
callTransfer i fv cargs si =  do  
  SEnv ptM paraMap initM ctM _ _ outS unTbl <- getSymExeEnv
  (csbvs,si') <- valuesToSbvs si cargs
  case valueContent' fv of
    FunctionC f -> do
      let fID = functionUniqueId f
          frmlIds = map valueUniqueId (functionParameters f)
          argMap = IM.filterWithKey mapF paraMap
            where mapF n (v,k) = k == -1 || elem n frmlIds
          -- (argVals,argIxs) = unzip $ IM.elems argMap
          -- argStrs = map toVarName' argVals
          -- argIds  = map valueUniqueId argVals
          -- (initM1, initM2) = IM.partitionWithKey (\k _ -> k `elem` argIds) initM
          -- argSbvs =  IM.toList initM
          initM' = IM.mapWithKey mapF initM
            where mapF n sbv = case IM.lookup n argMap of  
                       Just (gv, -1) -> if S.null gsbv then sbv else gsbv
                                  where gsbv = findVar gv si'
                       Just (v, k)   -> if S.null csbv then sbv else csbv
                                  where csbv = csbvs !! k
                       _             -> sbv
          constrs = [ argSbv .== actSbv | 
                      (argSbv, actSbv) <- zip (IM.elems initM) (IM.elems initM'),
                      not $ S.null actSbv ]
      lift . sequence_ $! map softConstrain constrs   -- Constrain
      -- argSbvs <- mapM mkValSbv argVals 
      -- let assigns = [ (argSbv, getActSbv argStr k) 
      --               | (argSbv, argStr, k) <- zip3 argSbvs argStrs argIxs]
      --     getActSbv argStr k = if k == -1 
      --                          then fromMaybe nilSExpr $! lkpVar argStr si'
      --                          else csbvs !! k
      --     !constrs = [argSbv .== actSbv | (argSbv, actSbv) <- assigns, not $ S.null actSbv]

      let !procTblFn = IM.findWithDefault (const M.empty) fID ptM 
          procSbvs = procTblFn $ IM.elems initM  
          procSbvs' = procTblFn $ IM.elems initM' 
          isOutArg = isArgOut outS
          !chgActArgs = [ ( toVarName' (toActVar v k) 
                          , fromMaybe nilSExpr $ M.lookup (toVarName' v) procSbvs')  
                        | (v,k) <- IM.elems argMap, isOutArg v ] 
            where toActVar v k = if k == -1 then v else memAccessBase (cargs !! k) 
          (argNames,outSbvs) = unzip $ filter ((not. S.null). snd) chgActArgs
         
          -- fRetSbv0 =  case functionExitInstruction f of
          --       Just inst@PhiNode { phiIncomingValues = ivs }  ->  res
          --          where  -- iCond = (sAnd constrs) .&& (getInstBlkCond inst ctM)
          --             res = S.unions $ map toSbv ivs
          --             toSbv (v,bb) = ite (chkBlk bb) (getSbv v) nilSExpr
          --             getSbv v = S.unions [cnstSbv v, findVar v si, findId v si] 
          --             cnstSbv v = maybe nilSExpr intToSExpr $ fromConstantInt v 
          --             chkBlk b = sAnd constrs .&& 
          --                        IM.findWithDefault sTrue (valueUniqueId b) ctM
          --       _   ->  M.findWithDefault nilSExpr (toVarName' f) procSbvs'              
          fRetSbv = M.findWithDefault nilSExpr (toVarName' f) procSbvs'
      let si2 = (varValueTbl %~ M.unionWithKey mapF procSbvs') si
            where mapF k vs' vs = if (head k == '@') && not (S.null vs') 
                                  then vs' else S.union vs' vs
          si3 = updCond f (sAnd constrs)  $!
                updVars argNames outSbvs si'
          si4 = if M.null procSbvs 
                then updValId i fRetSbv si
                else updValId i fRetSbv si3
      return si4

    ExternalFunctionC ef      
      | isMemCMS ef   -> 
          if length cargs < 2 then return si
          else updSymExeInfo i (cargs!!0,cargs!!1) si 
      | isC99Scanf ef -> do
          -- sbvs <- mapM mkExTypeSbv $ externalFunctionParameterTypes ef
          let cvals = map memAccessBase $ tail cargs
          sbvs <- mapM mkValSbv_ cvals
          let paras = map toVarName' cvals
              -- (paras', sbvs') = unzip $ zip paras sbvs
              si' = xtdInst (-2) (S.unions sbvs) $! updVars paras sbvs si
          return si'   
      | isC99Read ef  ->  
          if null cargs then return si
          else do
              let val = memAccessBase $ head cargs 
              sbv <- mkValSbv_ val
              return $! xtdInst (-2) sbv $! updValStr val sbv si                      
      | isFree ef  ->
          if null cargs || any isConstantPointerNull cargs 
          then return si
          else do
              let val = memAccessBase $ head cargs
                  si2 = updVar (toVarName' val) nilSExpr si
              return si2      
      | otherwise     -> do
          let res = exFuncToSbvOp ef csbvs
              !si2 = updValId i res si'
          if S.null res then return si else return si2
    _ -> return si      

identifySymExe ::    
       (FuncLike funcLike, HasFunction funcLike, HasCFG funcLike, 
        HasCDG funcLike, HasDomTree funcLike)
        => Module -> IndirectCallSummary 
        -> Lens' compositeSummary SymExeSummary
        -> Getter compositeSummary OutputSummary
        -> ComposableAnalysis compositeSummary funcLike
identifySymExe m ics = composableDependencyAnalysisM runner symExeAnalysis 
  where
    runner a = runSymExecutor a constData 
    constData = SEnv mempty pm undefined undefined undefined undefined undefined ust
    pm = genAllParaMap m
    ust = genUnsignedVarsMap m
    
symExeAnalysis :: (FuncLike funcLike, HasCFG funcLike,HasFunction funcLike, 
                   HasCDG funcLike, HasDomTree funcLike)
               => OutputSummary -> funcLike 
               -> SymExeSummary -> SymExecutor SymExeSummary
symExeAnalysis outS funcLike s@(SymExeSummary vs ptM ct initM) = do   
  -- liftIO $ putStrLn ("``` Now begin to analysis the function " ++ toVarName' f ++ " ...")    
  paraMap <- symExeEnvironment paraValMap
  let (paraIds, paraVals) = unzip. IM.toList $ IM.map fst paraMap
  -- liftIO . print $ map toVarName' paraVals    
  !paraSbvs <- if IM.null initM 
              then mapM mkValSbv paraVals
              else return $ IM.elems initM 
  let !initM' = if not (IM.null initM) then initM
               else IM.fromList $ zip paraIds paraSbvs      
  -- let globalVals = IM.elems $ IM.filter ((== -1) . snd) paraMap  
  --     fParaVals = frmlVals ++ globalVals   -- ++ allocVals f  
  --     (fParaVs, fParaKs) = unzip fParaVals 
  --     fParaStrs = map toVarName' fParaVs 
  --     fParaIds = map valueUniqueId fParaVs
  --     fParaSbvs = IM.elems $ IM.filterWithKey (\k _ -> k `elem` fParaIds) initM'
  -- fParaSbvs <- mapM mkValSbv fParaVs   -- symbolics
  let envMod e = e { procSymExeTbl = ptM
                   , argSbvMap = initM'   
                   , blkCondMap = ct
                   , controlDepGraph = getCDG funcLike
                   , domTree = getDomTree funcLike
                   , outSumm = outS
                   }      
      !fact0 = top         -- xtdVars fParaStrs fParaSbvs top  
      analysis = fwdDataflowAnalysis top meetSymExeInfo symExeTransfer  

  localInfo <- symExeLocal envMod (dataflow funcLike analysis fact0)  
  let SInfo tmpTbl varTbl condTbl = dataflowResult localInfo
      vs'  = M.unionWith mrgSExpr varTbl vs
      ct'  = IM.unionWith (.||) condTbl ct
      tmpM = IM.filterWithKey (\k _ -> k < 0) tmpTbl
      initM2 = IM.unionWith mrgSExpr initM' tmpM
      ptM' = IM.insertWith mrgVarFnTbl fID (\piArgs -> varTbl) ptM 
      mrgVarFnTbl tf1 tf2 = \piArgs -> 
          (M.unionWith mrgSExpr) (tf1 $ IM.elems initM) (tf2 $ IM.elems initM2)
      piArgs = IM.elems initM2
  return $ SymExeSummary vs' ptM' ct' initM2  
 where
    f = getFunction funcLike 
    fID = functionUniqueId f   
    frmlVals  = zip (map toValue $ functionParameters f) [0..]

{-
type IsParallel = Bool
computeSymExe :: IsParallel -> Module -> SymExeSummary
computeSymExe isPar m = ChkSumm._symExeSummary res
  where
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    ds = unsafePerformIO $ loadDependencies [] []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyEscapes ds ics escapeSummary
               , identifyOutput m ds outputSummary
              --  , identifySAPPTRels ds sapPTRelSummary
               , identifySAPs ds ics sapSummary sapPTRelSummary finalizerSummary
               , identifySymExe m ics symExeSummary sapSummary escapeSummary outputSummary 
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    !res = cgTraversal cg analysisFunc mempty 
    cgTraversal = if isPar then parallelCallGraphSCCTraversal else callGraphSCCTraversal
-}


---------

valueToSbv ::  Value -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
valueToSbv v0 si = do  
  -- ics <- symExeEnvironment icsSumm
  initM <- symExeEnvironment argSbvMap
  let !v' = stripBitcasts v0    -- ignoreCasts
  case valueContent' v0 of
    GlobalVariableC gv -> do
       let gvName = show (globalVariableName gv)    -- toVarName' gv
       case (lkpVar gvName si, globalVariableInitializer gv) of 
         (Just sbv, _) -> return (sbv,si) 
         (Nothing, Just bv)  -> updVarValSbv gv bv si  
         _   -> case IM.lookup (valueUniqueId gv) initM of 
               Just sbv -> return (sbv,si)
               Nothing  -> return (nullSExpr, si)    -- newSExpr gv si 
    ExternalValueC ev ->  
       case (lkpValStr ev si, IM.lookup (valueUniqueId ev) initM) of 
         (Just sbv, _) -> return (sbv,si) 
         (Nothing, Just sbv) -> return (sbv,si) 
         _  -> newSExpr ev si  -- return (nilSExpr, si)

    ArgumentC av -> 
       case (lkpValStr av si, IM.lookup (valueUniqueId av) initM) of 
         (Just sbv, _) -> return (sbv,si) 
         (Nothing, Just sbv) -> return (sbv,si) 
         _  -> newSExpr av si  -- return (nilSExpr, si)    

    GlobalAliasC ga -> valueToSbv (globalAliasTarget ga) si

    ConstantC c -> constToSbv c si
           
    FunctionC f ->  do     -- varValsToSList f (functionRets f) si   
       let brs = labelBlockReturns f
           blk = functionExitBlock f
       case (lkpValStr f si, blockReturns brs blk) of
          (Just sbv, _)  -> return (sbv,si) 
          (Nothing, Just vs)  -> updVarValsToSList f vs si    
              --  let sbv = S.unions $ map (flip findId si) vs
              --      si2 = updValStr f sbv si
              --  return (sbv, si2)   
          _       -> do
              sbv <- mkExTypeSbv (functionReturnType f)
              let si' = xtdInst (-1) sbv $! updValStr f sbv si
              return (sbv, si')

    ExternalFunctionC ef 
       |isExtFuns ["@pi"] ef -> 
           return (fromDouble (pi :: Double), si)
       |otherwise -> 
         case lkpValStr ef si of
          Just sbv   -> return (sbv,si) 
          Nothing    -> do
              let TypeFunction rt _ _ = externalFunctionType ef
              sbv <- mkExTypeSbv rt
              let si' = xtdInst (-1) sbv $! updValStr ef sbv si
              return (sbv, si')

    BasicBlockC bb -> do 
       let brs = labelBlockReturns f
           f   = basicBlockFunction bb
       case (lkpValId bb si, blockReturns brs bb) of
          (Just sbv, _)  -> return (sbv,si) 
          (Nothing, Just vs)  -> do    -- updTempValsToSList bb vs si    
               let sbv = S.unions $ map (flip findId si) vs
                   si2 = updValId bb sbv si
               return (sbv, si2)   
          _       ->  return (nilSExpr, si)

    InstructionC i  ->
      case i of 
        AllocaInst {} -> case (lkpValStr i si, IM.lookup (valueUniqueId i) initM) of 
            (Just sbv, _) -> return (sbv,si) 
            (Nothing, Just sbv) -> return (sbv,si) 
            _  -> newSExpr i si  -- return (nilSExpr, si)

        LoadInst {loadAddress = ptr} ->  valueToSbv (getPtrValue ptr) si
          -- valuesToSList i (indirectCallInitializers ics v) si   

        TruncInst { castedValue = cv } -> valueToSbv cv si
        ZExtInst { castedValue = cv } -> valueToSbv cv si
        SExtInst { castedValue = cv } -> valueToSbv cv si
        FPTruncInst { castedValue = cv } -> valueToSbv cv si
        FPExtInst { castedValue = cv } -> valueToSbv cv si
        FPToSIInst { castedValue = cv } -> valueToSbv cv si
        FPToUIInst { castedValue = cv } -> valueToSbv cv si
        SIToFPInst { castedValue = cv } -> valueToSbv cv si
        UIToFPInst { castedValue = cv } -> valueToSbv cv si
        PtrToIntInst { castedValue = cv } -> valueToSbv cv si
        IntToPtrInst { castedValue = cv } -> valueToSbv cv si
        BitcastInst { castedValue = cv } -> do
            (sbv, si') <- valueToSbv cv si
            let structSize = typeSize $ valueType i
                iCond = getCurrCond i si
                isIxOut = fromSBool $ sbv .>= intToSExpr structSize
                rule3_Sbv = ite iCond isIxOut nilSExpr
                si2 = if isStructType $ valueType i
                      then updVar ("!_#3_" ++ toVarName' i) rule3_Sbv si'
                      else si'
            return (sbv, si2)

        AddInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            fpOrIntOpToSbv fpAdd (+) lhs rhs i si
            -- if any isFpDbType $ map valueType [toValue i, lhs, rhs] 
            -- then fpOpToSbv fpAdd lhs rhs i si else
            --      binOpToSbv (+) lhs rhs i si         
        SubInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            fpOrIntOpToSbv fpSub (-) lhs rhs i si
            -- if any isFpDbType $ map valueType [toValue i, lhs, rhs]
            -- then fpOpToSbv fpSub lhs rhs i si else
            --      binOpToSbv (-) lhs rhs i si         
        MulInst { binaryLhs = lhs, binaryRhs = rhs } ->
            fpOrIntOpToSbv fpMul (*) lhs rhs i si
            -- if any isFpDbType $ map valueType [toValue i, lhs, rhs] 
            -- then fpOpToSbv fpMul lhs rhs i si else
            --      binOpToSbv (*) lhs rhs i si        
        DivInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            fpOrIntOpToSbv fpDiv sDiv lhs rhs i si
            -- if any isFpDbType $ map valueType [toValue i, lhs, rhs] 
            -- then fpOpToSbv fpDiv lhs rhs i si else
            --      binOpToSbv sDiv lhs rhs i si    
        RemInst { binaryLhs = lhs, binaryRhs = rhs } ->
            fpOrIntOpToSbv (const fpRem) sRem lhs rhs i si
            -- if any isFpDbType $ map valueType [toValue i, lhs, rhs] 
            -- then fpOpToSbv (const fpRem) lhs rhs i si else
            --      binOpToSbv sRem lhs rhs i si       
        AndInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            binOpToSbv (.&.) lhs rhs i si    
        OrInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            binOpToSbv (.|.) lhs rhs i si    
        XorInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            binOpToSbv xor lhs rhs i si    
        ShlInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            shiftOpToSbv op lhs rhs i si
          where op = sShiftLeft :: SInt32 -> SInt32 -> SInt32
        LshrInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            shiftOpToSbv op lhs rhs i si
          where op = sShiftRight :: SInt32 -> SInt32 -> SInt32
        AshrInst { binaryLhs = lhs, binaryRhs = rhs } -> 
            if isUnsigned lhs 
            then shiftOpToSbv op1 lhs rhs i si
            else shiftOpToSbv op2 lhs rhs i si
          where 
            op1 = sSignedShiftArithRight :: SInt32 -> SInt32 -> SInt32
            op2 = sShiftRight :: SInt32 -> SInt32 -> SInt32

        ICmpInst { cmpPredicate = cmp, cmpV1 = v1, cmpV2 = v2 } ->
            fpOrIntCmpToSbv False cmp v1 v2 i si
            -- cmpOpToSbv cmp v1 v2 i si
        FCmpInst { cmpPredicate = cmp, cmpV1 = v1, cmpV2 = v2 } ->
            fpOrIntCmpToSbv True cmp v1 v2 i si
            -- fcmpOpToSbv cmp v1 v2 i si

        PhiNode { phiIncomingValues = ivs } -> 
          -- tempValsToSList i (filter (not . isPhiNode) $ flattenValue v0) si              
            case lkpValId i si of
               Just sbv   ->  return (sbv, si)
               _          ->  do                 
                   (sbv1, si1) <- phiToSbv i ivs si
                   if not (S.null sbv1) then return (sbv1, si1)
                   else do 
                      (sbv2, si2) <- phiToSbv2 i ivs si
                      if not (S.null sbv2) then return (sbv2, si2)
                      else phiToSbv3 i ivs si
               

        SelectInst { selectTrueValue = t, selectFalseValue = f, 
                     selectCondition = c } -> 
          case lkpValId i si of 
            Just sbv -> return (sbv,si) 
            Nothing  -> do
                ([cv,tv,fv], si') <- valuesToSbvs si [c,t,f] 
                let !res = ite (toSBool cv) tv fv  
                    si2 = updInst i res si'
                return (res, si2)             

        BranchInst { branchTrueTarget = tb, branchFalseTarget = fb, 
                     branchCondition = c } -> do
            (cv,si') <- valueToSbv (memAccessBase c) si  
            let cCond = toSBool cv     -- toSBool2
                iCond = getCurrCond i si
                !tCond = cCond .&& iCond
                !fCond = sNot cCond .&& iCond
                -- preBlk = case instructionBasicBlock i of
                --           Just bb  -> intToSExpr $ valueUniqueId bb
                --           Nothing  -> nilSExpr
                -- si1 = if S.null preBlk then si' 
                --       else updVar "#0#preBlk" preBlk si'
                si2 = updConds [tb,fb] [tCond,fCond] si'
            ([tv,fv], si2') <- valuesToSbvs si2 [toValue tb,toValue fb] 
            let res = ite tCond tv fv  
                chkDeadLoop = fromSBool $ cv .== trueSExpr
                isDeadLoop = ite tCond chkDeadLoop nilSExpr
                si3 = if sbvToBool cCond 
                      then updVar ("!_#5_" ++ toVarName' i) isDeadLoop si2'
                      else si2'
            return (res, si3)                 

        SwitchInst { switchCases = vbbs, switchDefaultTarget = bb0, 
                     switchValue = v } -> do
            let (vs, bbs) = unzip vbbs  
            (v':vs', si') <- valuesToSbvs si (v:vs)
            let bb = fromMaybe bb0 $! lookup v' (zip vs' bbs)
                !iCond = getCurrCond i si  
                vConds = map ((iCond .&&). (anyEqSExpr v')) vs'
                -- preBlk = case instructionBasicBlock i of
                --           Just bb  -> intToSExpr $ valueUniqueId bb
                --           Nothing  -> nilSExpr
                -- si1 = if S.null preBlk then si' 
                --       else updVar "#0#preBlk" preBlk si'
                si2 = updConds (bb0:bbs) (iCond:vConds) si'
            valueToSbv (toValue bb) si2                     

        IndirectBranchInst { indirectBranchAddress = ptr 
                           , indirectBranchTargets = bbs } -> do
            (v, si') <- valueToSbv (memAccessBase ptr) si   
            let mbBlk = do 
                   v' <- if S.null v then Nothing
                         else unliteral $ S.elemAt 0 v 
                   let vName = show v'
                       chkF b = identifierAsString (basicBlockName b) == vName
                   find chkF bbs 
                !iCond = getCurrCond i si
                -- preBlk = case instructionBasicBlock i of
                --           Just bb  -> intToSExpr $ valueUniqueId bb
                --           Nothing  -> nilSExpr
                -- si1 = if S.null preBlk then si' 
                --       else updVar "#0#preBlk" preBlk si'
                -- vConds = replicate (length bbs) iCond
                -- si2 = updConds bbs vConds si'
            case mbBlk of 
               Just bb   ->  valueToSbv (toValue bb) $!
                             updCond bb iCond si'
               Nothing   ->  return (nilSExpr, si)                     

        UnconditionalBranchInst { unconditionalBranchTarget = bb } -> do
            let !iCond = getCurrCond i si
                -- preBlk = case instructionBasicBlock i of
                --           Just bb  -> intToSExpr $ valueUniqueId bb
                --           Nothing  -> nilSExpr
                -- si1 = if S.null preBlk then si 
                --       else updVar "#0#preBlk" preBlk si
                si2 = updCond bb iCond si
            valueToSbv (toValue bb) si2                       

        LandingPadInst { landingPadPersonality = p
                       , landingPadClauses = vlpcs } -> do
            let (vs,lpcs) = unzip vlpcs
            tempValsToSList i (p:vs) si 
            -- let vlpcs' = mapFsts getValueName vlpcs 
            -- case lookup (getValueName p) vlpcs' of
            --   Just lpc  ->  return (toSExpr' $ show lpc, si)  
            --   Nothing   ->  return (nilSExpr, si)
        
        ExtractElementInst { extractElementVector = v
                           , extractElementIndex = ix } ->     
            case (lkpValId i si, getVecVal v ix) of 
              (Just sbv, _) -> return (sbv,si)
              (Nothing, Just tv)  -> updTempValSbv i tv si  
              _      -> return (nilSExpr, si)

        InsertElementInst { insertElementVector = v, insertElementIndex = iv
                          , insertElementValue = val } -> 
          case lkpValId i si of 
            Just sbv -> return (sbv,si) 
            Nothing  -> do  
                let vs = insertVecVal v val iv
                if null vs then return (nilSExpr, si)
                else updTempValsToSList i vs si   

        ShuffleVectorInst { shuffleVectorV1 = v1, shuffleVectorV2 = v2
                          , shuffleVectorMask = m } -> 
          case lkpValId i si of 
            Just sbv -> return (sbv,si) 
            Nothing  -> do   
                let ixs = mapMaybe fromConstantInt $ getVecValues m
                    vs1 = getVecValues v1
                    vs2 = getVecValues v2
                    vs = [(vs1 ++ vs2) !! k | k <- ixs]
                if null vs then return (nilSExpr, si)
                else updTempValsToSList i vs si    

        ExtractValueInst { extractValueAggregate = a
                         , extractValueIndices = ixs } ->      
            case (lkpValId i si, getAggVal a ixs) of 
              (Just sbv, _) -> return (sbv,si)
              (Nothing, Just tv)  -> updTempValSbv i tv si  
              _      -> return (nilSExpr, si)     
        
        InsertValueInst { insertValueAggregate = a, insertValueValue = v
                        , insertValueIndices = ixs } -> 
          case lkpValId i si of 
            Just sbv -> return (sbv,si) 
            Nothing  -> do    
                let vs = insertAggVal a v ixs
                if null vs then return (nilSExpr, si)
                else updTempValsToSList i vs si   

        GetElementPtrInst { getElementPtrValue = v
                          , getElementPtrIndices = ixs } -> 
            tempValToSbv i (getGepFinalVal v ixs) si
            -- case getGepFinalSbv v ixs si of
            --   Just sbv  -> return (sbv, si)
            --   _   -> valueToSbv (getGepFinalVal v ixs) si                            
        
        ResumeInst { resumeException = e } -> tempValToSbv i (toValue e) si  
        VaArgInst { vaArgValue = v } -> tempValToSbv i v si

        UnreachableInst { } -> return (nilSExpr, si)
        FenceInst { }  -> return (nilSExpr, si)
        
        RetInst { retInstValue = rv } -> -- tempValToSbv i rv si 
           case (lkpValId i si, rv, instructionFunction i) of 
              (Just sbv, _, _)  -> return (sbv, si)
              (Nothing, Just v, Just f)   ->  do
                    (sbv, si') <- valueToSbv v si
                    let si2 = updValId i sbv $!
                              updVar (toVarName' f) sbv si'
                    return (sbv, si2)
              --  case valueContent' v of
              --   InstructionC inst@PhiNode { phiIncomingValues = ivs } -> do
              --       (sbv, si') <- phiToSbv i ivs si
              --       -- let (vs,bbs) = unzip ivs
              --       -- (sbvs, si') <- valuesToSbvs si vs 
              --       -- let chkBlk b = fromMaybe sTrue $ lkpCond b si  
              --       --     res = S.unions . map toSbv $ zip sbvs bbs
              --       --     toSbv (sbv,bb) = ite (chkBlk bb) sbv nilSExpr
              --       let si2 = updVar (toVarName' f) sbv si'
              --       return (sbv, si2)
              (Nothing, Just v, Nothing) -> updTempValSbv i v si
              _  ->  return (nilSExpr, si) 

        CallInst { callFunction = fv } -> tempValToSbv i fv si    
        InvokeInst { invokeFunction = fv } -> tempValToSbv i fv si

        _  -> return (findId i si, si)
            -- case (lkpInst i si, instRets i) of
            --   (Just sbv, _)  -> return (sbv,si) 
            --   (Nothing, Just vs)  -> updTempValsToSList i vs si    
            --   _       ->  return (nilSExpr, si)


constToSbv :: Constant -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
constToSbv cnst si = case cnst of 
   ConstantInt { constantIntValue = iv } -> 
      return (intToSExpr iv, si)
   ConstantString { constantStringValue = txt } -> 
      return (toSExpr' $! T.unpack txt, si)
   ConstantFP { constantFPValue = d } -> 
      return (fromDouble d, si)     -- toSExpr'

   ConstantStruct {constantStructValues = vs} -> 
      tempValsToSList (toValue cnst) vs si
   ConstantArray {constantArrayValues = vs} -> 
      tempValsToSList (toValue cnst) vs si
   ConstantVector {constantVectorValues = vs} -> 
      tempValsToSList (toValue cnst) vs si

   ConstantAggregateZero {}  -> return (nilSExpr, si)
   ConstantPointerNull {}  ->  return (nilSExpr, si)
   UndefValue {} ->  return (nilSExpr, si)

   ConstantValue { constantInstruction = (valueContent' ->
       InstructionC i@GetElementPtrInst { getElementPtrValue = (valueContent' ->
          GlobalVariableC GlobalVariable { globalVariableInitializer = Just v }),
              getElementPtrIndices = (valueContent -> 
                 ConstantC ConstantInt { constantIntValue = 0 }):ixs })} -> 
      case (lkpValId cnst si, resolveInitializer v ixs) of
        (Just sbv, _)      -> return (sbv, si)
        (Nothing, Just v)  -> updTempValSbv (toValue cnst) v si
        _                  -> return (nilSExpr, si)
   ConstantValue { constantInstruction  = inst } -> 
      valueToSbv (memAccessBase $ toValue inst) si

   InlineAsm { inlineAsmString = t1,  inlineAsmConstraints = t2} -> 
      return (nilSExpr, si)
   BlockAddress { blockAddressFunction = f, blockAddressBlock  = bb } -> do 
      let brs = labelBlockReturns f
      case blockReturn brs bb of
        Just rv  ->  valueToSbv rv si
        _        ->  return (nilSExpr, si)


------------------------------
--- Some Utils  
valuesToSList :: SymExeInfo -> [Value] -> SymExecutor (SymExeExpr, SymExeInfo)
valuesToSList si vs = do
   (sbvs, si') <- valuesToSbvs si vs
   let sbv = S.unions sbvs      -- toSList sbvs
   return (sbv, si')

valuesToSbvs :: SymExeInfo -> [Value] -> SymExecutor ([SymExeExpr], SymExeInfo)
valuesToSbvs si = foldM actionF ([], si) . reverse    -- mapM (`valueToSbv` si)  
 where   
   actionF (bs,si0) v =  do
      (sbv, si') <- valueToSbv v si0
      return (sbv:bs, si')

tempValsToSList, updTempValsToSList :: IsValue a => 
        a -> [Value] -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
tempValsToSList x vs si = 
   case lkpValId x si of 
      Just sbv -> return (sbv,si) 
      Nothing  -> updTempValsToSList x vs si

updTempValsToSList x vs si = do
    (sbv, si') <- valuesToSList si vs
    let si2 = updValId x sbv si'
        -- sbv' = S.take 30 sbv     -- 
    return (sbv, si2)    

tempValToSbv, updTempValSbv :: IsValue a => 
        a -> Value -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
tempValToSbv x v si = 
    case lkpValId x si of 
       Just sbv -> return (sbv,si) 
       Nothing  -> updTempValSbv x v si 

updTempValSbv a v si = do
    (sbv, si') <- valueToSbv v si
    let si2 = updValId a sbv si'
    return (sbv, si2)

varValsToSList, updVarValsToSList :: IsValue a => 
        a -> [Value] -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
varValsToSList x vs si = 
   case lkpValStr x si of 
      Just sbv -> return (sbv,si) 
      Nothing  -> updVarValsToSList x vs si

updVarValsToSList x vs si = do
    (sbv, si') <- valuesToSList si vs
    let si2 = updValStr x sbv si' 
    return (sbv, si2)   

varValToSbv, updVarValSbv :: IsValue a =>
        a -> Value -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
varValToSbv x v si = do
    case lkpValStr x si of 
       Just sbv -> return (sbv,si) 
       Nothing  -> updVarValSbv x v si 

updVarValSbv x v si = do
    (sbv, si') <- valueToSbv v si
    let si2 = updValStr x sbv si'
    return (sbv, si2)


phiToSbv :: Instruction -> [(Value, Value)] -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
phiToSbv i ivs si = do
  cdg <- symExeEnvironment controlDepGraph
  dt <- symExeEnvironment domTree
  let cdeps = directControlDependencies cdg i
      vs = map fst ivs
  case cdeps of
    [] -> phiToSbv2 i ivs si
    [_] -> do
      let predTerms = mapMaybe toPreds ivs
          nonBackedges = filter (isNotBackedge dt i) predTerms
      case filter (isUnconditional . snd) nonBackedges of
        [] ->  phiToSbv2 i ivs si
        [(v,_)] -> updTempValSbv i v si
        _ -> phiToSbv2 i ivs si
    _ -> phiToSbv2 i ivs si
  where
    toPreds (v,bb) = do
       b <- fromValue bb
       let ib = basicBlockTerminatorInstruction b
       return (v,ib)
    isUnconditional UnconditionalBranchInst {} = True
    isUnconditional _ = False
    isNotBackedge g inst (_, br) = not (dominates g inst br)

phiToSbv2 i ivs si = case phi2Var (toValue i) of 
   Just v  -> updTempValSbv i v si
   Nothing -> do
      let vsbv = S.unions $ mapMaybe toSbv ivs
          toSbv (v,bb) = do
              bCond <- lkpCond bb si 
              return $ ite bCond (getSbv v) nilSExpr
          getSbv v = S.unions [cnstSbv v, findVar v si, findId v si]
          cnstSbv v = maybe nilSExpr intToSExpr $ fromConstantInt v
      let si2 = updValId i vsbv si
      return (vsbv, si2)

phiToSbv3 i ivs si = case phi2Var (toValue i) of 
   Just v  -> updTempValSbv i v si
   Nothing -> do
      let cvs = filter isConstant $ flattenValue (toValue i)
      -- (sbv3, si') <- valuesToSList si cvs
      (sbvs, si') <- valuesToSbvs si cvs 
      let sbv3 = S.unions [ sbv | (cv, sbv) <- zip cvs sbvs
                          , (v,bb) <- ivs, v == cv, chkBB bb ]          
          chkBB b = fromMaybe True $ do
              bCond <- lkpCond b si 
              return $ sbvToBool bCond
          si3 = updValId i sbv3 si'
      return (sbv3, si3)

---
typeConvert :: IsValue a => Value -> a -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
typeConvert cv i si =  case lkpValId i si of 
    Just sbv -> return (sbv,si) 
    Nothing  -> do
        (sbv, si') <- valueToSbv cv si
        let sbv' = case (simpleValueType i, simpleValueType cv) of 
                (TypeFloat, _)  -> S.map (unsafeCastSBV. toSFloat sRNE) sbv
                (TypeDouble, _) -> S.map (unsafeCastSBV. toSDouble sRNE) sbv
                (TypeInteger _, TypeFloat) -> S.map (fromSFloat sRNE. unsafeCastSBV) sbv
                (TypeInteger _, TypeDouble) -> S.map (fromSDouble sRNE. unsafeCastSBV) sbv
                (TypeInteger _, TypeInteger _) -> S.map sFromIntegral sbv
                _   -> S.map unsafeCastSBV sbv
            si2 = updValId i sbv' si'
        return (sbv', si2)  


fpOrIntOpToSbv :: (SRoundingMode -> SDouble -> SDouble -> SDouble)
            -> (SInt32 -> SInt32 -> SInt32)
            -> Value -> Value -> Instruction -> SymExeInfo
            -> SymExecutor (SymExeExpr, SymExeInfo)
fpOrIntOpToSbv fop op lhs rhs i si =  case lkpValId i si of 
    Just sbv -> return (sbv,si) 
    Nothing  -> do   
      ([lv,rv], si') <- valuesToSbvs si $ map memAccessBase [lhs,rhs]
      let res = if fpChk 
                then S.fromList [runfOp l r | l <- S.toList lv, r <- S.toList rv] 
                else S.fromList [runOp l r | l <- S.toList lv, r <- S.toList rv]
          fpChk = any isFpDbType $ map simpleValueType [toValue i, lhs, rhs]
                  -- any isFpDblValue (S.toList lv ++ S.toList rv) 
          runfOp l r = fromSDouble sRNE $ 
                       (fop sRNE) (toSDouble sRNE l) (toSDouble sRNE r) 
          runOp l r = forceToIntSbv l `op` forceToIntSbv r
          si2 = updInst i res si'   

      si3 <- addChkDiv0 (rhs,rv) i si2    
      si4 <- addChkOverflow (lv,rv) i si3
      return (res, si4)

addChkDiv0 :: (Value, SymExeExpr) -> Instruction -> SymExeInfo -> SymExecutor SymExeInfo
addChkDiv0 (v0,rv) i si 
   | isDivRemInst i = do
        let !iCond = getCurrCond i si
            bCond b = fromMaybe sTrue $ lkpCond b si
            getSbv v = S.unions [cSbv v, findVar v si, findId v si]
            cSbv v = maybe nilSExpr intToSExpr $ fromConstantInt v
            chkSbv0 sbv = sOr [ forceToIntSbv bv .== 0 | bv <- S.toList sbv ]
            chkV0 v = chkSbv0 $ getSbv v
            toSbool (v,b) = iCond .&& (bCond b) .&& chkV0 v
            rvSbool = case valueContent' v0 of
                InstructionC PhiNode { phiIncomingValues = ivs } -> 
                    fromSBool . sOr $ map toSbool ivs
                _  -> fromSBool $ iCond .&& (chkSbv0 rv)
            rvSbool' = ite iCond (fromSBool $ chkSbv0 rv) nilSExpr
            rvSbv = ite iCond rv nilSExpr
            si2 = updVar ("!_#6_" ++ toVarName' i) rvSbv si
#ifdef ZYZ_DEBUG
        liftIO . putStrLn $ printf "%s\"%s\": Checking Division by Zero ..." 
                                  (show $ toSrcLnStr' [toValue i]) (show i)
#endif
        return si2

   | otherwise =  return si

addChkOverflow :: (SymExeExpr, SymExeExpr) -> Instruction -> SymExeInfo -> SymExecutor SymExeInfo
addChkOverflow (lv,rv) i si =  case i of
   AddInst { binaryLhs = lhs, binaryRhs = rhs } -> chkOverflow bvAddO lhs rhs
   SubInst { binaryLhs = lhs, binaryRhs = rhs } -> chkOverflow bvSubO lhs rhs   
   MulInst { binaryLhs = lhs, binaryRhs = rhs } -> chkOverflow bvMulO lhs rhs 
   DivInst { binaryLhs = lhs, binaryRhs = rhs } -> chkOverflow bvDivO lhs rhs   
   _  -> return si   
 where  
   chkOverflow opO lhs rhs = do
      utbl <- symExeEnvironment unsignsTbl
      let (underflows,overflows) = unzip [ forceToIntSbv l `opO` forceToIntSbv r 
                                         | l <- S.toList lv, r <- S.toList rv]  
          chkOpOverflow = sOr $ underflows ++ overflows
          -- chkValOver (v,sbv) = (S.map forceToIntSbv sbv .< zeroSExpr) .&& 
          --        (fromBool $ isPointerType (memAccessBase v) || isUnsignedVar i)                             
          isOverflow = fromSBool $ sAny (chkValOverflow utbl) 
                       [(lvUnsign, lhs,lv),(rvUnsign, rhs,rv)] .|| chkOpOverflow  
          iCond = getCurrCond i si
          lvUnsign = isUnsign lhs
          rvUnsign = isUnsign rhs
          isUnsign = or . map (chkVarTypeWith utbl) . refs
          ofSbv = ite iCond isOverflow nilSExpr   -- (intToSExpr 0 )
          si2 = if lvUnsign || rvUnsign 
                then updVar ("!_#8_" ++ toVarName' i) ofSbv si
                else si
      return si2 

chkValOverflow :: UnsignedVarTbl -> (Bool, Value, SymExeExpr) -> SBool
chkValOverflow utbl (isUnsign, v, sbv) = 
  case (isUnsign, simpleValueType v) of
    (True, TypeInteger n) -> chkIntOF n $ S.toList sbv   
    _ -> sFalse
  where 
    mrgOF (_, (uo,oo)) = uo .|| oo
    chkIntOF 8 = sOr . map (mrgOF . 
               (sFromIntegralO :: SInt32 -> (SWord8, (SBool,SBool))) )
    chkIntOF 16 = sOr . map (mrgOF . 
               (sFromIntegralO :: SInt32 -> (SWord16, (SBool,SBool))) )
    chkIntOF 32 = sOr . map (mrgOF . 
               (sFromIntegralO :: SInt32 -> (SWord32, (SBool,SBool))) )
    chkIntOF 64 = sOr . map (mrgOF . 
               (sFromIntegralO :: SInt32 -> (SWord64, (SBool,SBool))) )
    chkIntOF _ = sOr . map (mrgOF . 
               (sFromIntegralO :: SInt32 -> (SWord32, (SBool,SBool))) )
         


---
fpOrIntCmpToSbv :: Bool -> CmpPredicate  
            -> Value -> Value -> Instruction -> SymExeInfo
            -> SymExecutor (SymExeExpr, SymExeInfo)
fpOrIntCmpToSbv isFCmp cmp lhs rhs i si = case lkpValId i si of 
   Just sbv -> return (sbv,si) 
   Nothing  -> do      
      ([lv,rv], si') <- valuesToSbvs si [lhs,rhs]
      let res = if fpChk
                then fromSBool $ sOr [runfCmp l r | l <- S.toList lv, r <- S.toList rv] 
                else fromSBool $ sOr [runiCmp l r | l <- S.toList lv, r <- S.toList rv]
          fpChk = isFCmp || (any isFpDbType $ map simpleValueType [lhs, rhs])
          runfCmp l r = (cmpOpToSbvOp' cmp) (toSDouble sRNE l) (toSDouble sRNE r)
          runiCmp l r = (cmpOpToSbvOp cmp) (forceToIntSbv l) (forceToIntSbv r)             
          si2 = updInst i res si'
      return (res, si2) 


binOpToSbv ::  (SInt32 -> SInt32 -> SInt32)
            -> Value -> Value -> Instruction -> SymExeInfo
            -> SymExecutor (SymExeExpr, SymExeInfo)
binOpToSbv op lhs rhs i si = case lkpValId i si of 
   Just sbv -> return (sbv,si) 
   Nothing  -> do    
      ([lv,rv], si') <- valuesToSbvs si [lhs,rhs]
      let res = S.fromList [op l r | l <- S.toList lv, r <- S.toList rv]
          si2 = updInst i res si'
      return (res, si2)  

binOpToSbv' :: SymVal a => (SBV b1 -> SBV b2 -> SBV a)
            -> Value -> Value -> Instruction -> SymExeInfo
            -> SymExecutor (SymExeExpr, SymExeInfo)
binOpToSbv' op lhs rhs i si = do   
    ([lv,rv], si') <- valuesToSbvs si [lhs,rhs]
    let res = toSList [unsafeCastSBV l `op` unsafeCastSBV r 
                      | l <- S.toList lv, r <- S.toList rv ] 
        -- si2 = updInst i res si'
    return (res, si')  

fpOpToSbv ::  (SRoundingMode -> SDouble -> SDouble -> SDouble)   
            -> Value -> Value -> Instruction -> SymExeInfo
            -> SymExecutor (SymExeExpr, SymExeInfo)
fpOpToSbv op lhs rhs i si = case lkpValId i si of 
   Just sbv -> return (sbv,si) 
   Nothing  -> do   
      ([lv,rv], si') <- valuesToSbvs si [lhs,rhs]
      let res = S.fromList [runOp l r | l <- S.toList lv, r <- S.toList rv] 
          runOp l r = fromSDouble sRNE $ 
                       (op sRNE) (toSDouble sRNE l) (toSDouble sRNE r)
          si2 = updInst i res si'
      return (res, si2)

cmpOpToSbv :: CmpPredicate  -- (SInt32 -> SInt32 -> SBool)
            -> Value -> Value -> Instruction -> SymExeInfo
            -> SymExecutor (SymExeExpr, SymExeInfo)
cmpOpToSbv op lhs rhs i si = do   
    ([lv,rv], si') <- valuesToSbvs si [lhs,rhs]
    let res = fromSBool $ sOr [ cmpOpToSbvOp' op l r 
                              | l <- S.toList lv, r <- S.toList rv ] 
    return (res, si') 

fcmpOpToSbv :: CmpPredicate  -- (SInt32 -> SInt32 -> SBool)
            -> Value -> Value -> Instruction -> SymExeInfo
            -> SymExecutor (SymExeExpr, SymExeInfo)
fcmpOpToSbv op lhs rhs i si = do   
    ([lv,rv], si') <- valuesToSbvs si $ map memAccessBase [lhs,rhs]
    let res = fromSBool $ sOr [ runCmp op l r  -- `debug` ("cmp(l,r) =" ++ show (l,r))
                              | l <- S.toList lv, r <- S.toList rv ] 
        runCmp op l r = (cmpOpToSbvOp' op) (toSDouble sRNE l) (toSDouble sRNE r)                    
    return (res, si')  -- `debug` ("cmp(lv,rv) =" ++ show (lv,rv))

---
shiftOpToSbv :: (SymVal a, Num a, Num b, SymVal b) 
            => (SBV a -> SBV b -> SBV a)
            -> Value -> Value -> Instruction -> SymExeInfo
            -> SymExecutor (SymExeExpr, SymExeInfo)
shiftOpToSbv op lhs rhs i si = case lkpValId i si of 
   Just sbv -> return (sbv,si) 
   Nothing  -> do       
      ([lv,rv], si') <- valuesToSbvs si [lhs,rhs]
      let res = toSList [sFromIntegral l `op` sFromIntegral r 
                        | l <- S.toList lv, r <- S.toList rv,
                          isConcrete r ] 
          si2 = updInst i res si'
      return (res, si2)

exFuncToSbvOp :: (IsValue a) => a -> [SymExeExpr] -> SymExeExpr
exFuncToSbvOp ef args
  |isExtFuns ["@pi"] ef = toSExpr' (pi :: Float)
  |isExtFuns ["@exp"] ef = runUnaryOp exp arg1
  |isExtFuns ["@log"] ef = runUnaryOp log arg1
  |isExtFuns ["@sqrt"] ef = runUnaryOp sqrt arg1
  |isExtFuns ["@sin"] ef = runUnaryOp sin arg1
  |isExtFuns ["@cos"] ef = runUnaryOp cos arg1
  |isExtFuns ["@tan"] ef = runUnaryOp tan arg1
  |isExtFuns ["@asin"] ef = runUnaryOp asin arg1
  |isExtFuns ["@acos"] ef = runUnaryOp acos arg1
  |isExtFuns ["@atan"] ef = runUnaryOp atan arg1
  |isExtFuns ["@sinh"] ef = runUnaryOp sinh arg1
  |isExtFuns ["@cosh"] ef = runUnaryOp cosh arg1
  |isExtFuns ["@tanh"] ef = runUnaryOp tanh arg1
  |isExtFuns ["@asinh"] ef = runUnaryOp asinh arg1
  |isExtFuns ["@acosh"] ef = runUnaryOp acosh arg1
  |isExtFuns ["@atanh"] ef = runUnaryOp atanh arg1
  |isExtFuns ["@logBase"] ef = runBinOp logBase arg1 arg2
  |otherwise  = nilSExpr  -- error "Not implemented yet !"
 where arg1 = if null args then nilSExpr else head args
       arg2 = if length args < 2 then nilSExpr else args !! 1


runUnaryOp :: (SFloat -> SFloat) -> SymExeExpr -> SymExeExpr
runUnaryOp op arg = S.fromList [fromSFloat sRNE $ op (toSFloat sRNE v)
                               | v <- S.toList arg, isConcrete v]
  --  ite (S.null arg) nilSExpr $! toSExpr . op . unsafeCastSBV $ S.head arg

runBinOp :: (SFloat -> SFloat -> SFloat) -> SymExeExpr -> SymExeExpr -> SymExeExpr
runBinOp op arg1 arg2 = 
   S.fromList [fromSFloat sRNE $ toSFloat sRNE l `op` toSFloat sRNE r 
              | l <- S.toList arg1, r <- S.toList arg2, isConcrete l, isConcrete r ] 
  --  ite (S.null arg1 .|| S.null arg2) nilSExpr . toSExpr $!
  --    op (unsafeCastSBV $ S.head arg1) (unsafeCastSBV $ S.head arg2)


cmpOpToSbvOp' :: OrdSymbolic a => CmpPredicate -> a -> a -> SBool
cmpOpToSbvOp' p 
  | p `elem` [ICmpEq, FCmpOeq, FCmpUeq]  =  (.==)
  | p `elem` [ICmpNe, FCmpOne, FCmpUne]  =  (./=)
  | p `elem` [ICmpUge, ICmpSge, FCmpOge, FCmpUge]  =  (.>=)
  | p `elem` [ICmpUle, ICmpSle, FCmpOle, FCmpUle]  =  (.<=)
  | p `elem` [ICmpUgt, ICmpSgt, FCmpOgt, FCmpUgt]  =  (.>)
  | p `elem` [ICmpUlt, ICmpSlt, FCmpOlt, FCmpUlt]  =  (.<)
  | p == FCmpFalse   =  \ _ _ -> sFalse  
  | p == FCmpTrue    =  \ _ _ -> sTrue

cmpOpToSbvOp :: CmpPredicate -> SInt32 -> SInt32 -> SBool
cmpOpToSbvOp p l r
  | p == ICmpEq  =  forceToIntSbv l .== forceToIntSbv r
  | p `elem` [FCmpOeq, FCmpUeq]  =  toSFloat sRNE l .== toSFloat sRNE r
  | p == ICmpNe  =  forceToIntSbv l ./= forceToIntSbv r
  | p `elem` [FCmpOne, FCmpUne]  =  toSFloat sRNE l ./= toSFloat sRNE r
  | p `elem` [ICmpUge, ICmpSge]  =  forceToIntSbv l .>= forceToIntSbv r
  | p `elem` [FCmpOge, FCmpUge]  =  toSFloat sRNE l .>= toSFloat sRNE r
  | p `elem` [ICmpUle, ICmpSle]  =  forceToIntSbv l .<= forceToIntSbv r
  | p `elem` [FCmpOle, FCmpUle]  =  toSFloat sRNE l .<= toSFloat sRNE r
  | p `elem` [ICmpUgt, ICmpSgt]  =  forceToIntSbv l .> forceToIntSbv r
  | p `elem` [FCmpOgt, FCmpUgt]  =  toSFloat sRNE l .> toSFloat sRNE r
  | p `elem` [ICmpUlt, ICmpSlt]  =  forceToIntSbv l .< forceToIntSbv r
  | p `elem` [FCmpOlt, FCmpUlt]  =  toSFloat sRNE l .< toSFloat sRNE r
  | p == FCmpFalse   =  sFalse  
  | p == FCmpTrue    =  sTrue


---
getCurrCond :: Instruction -> SymExeInfo -> Constraint
getCurrCond i si = fromMaybe sTrue $ do
          bb <- instructionBasicBlock i 
          lkpCond i si

getInstBlkCond :: Instruction -> IntMap Constraint -> Constraint
getInstBlkCond i ctM = fromMaybe sTrue $ do
          bb <- instructionBasicBlock i 
          IM.lookup (valueUniqueId bb) ctM

{-
getVecSbv ::  Value -> Value -> SymExeInfo -> Maybe SymExeExpr
getVecSbv v ix si = do
  sbv <- lkpValId (valueContent' v) si 
  walkSList sbv [ix]

getAggSbv :: Value -> [Int] -> SymExeInfo -> Maybe SymExeExpr
getAggSbv v ixs si = do
  sbv <- lkpValId (valueContent' v) si 
  walkSList' sbv ixs

getGepFinalSbv :: Value -> [Value] -> SymExeInfo -> Maybe SymExeExpr
getGepFinalSbv base (ptrIx : ixs) si = 
  case (valueType base, lkpValId (memAccessBase base) si) of
    (TypePointer _ _, Just sbv) -> walkSList sbv ixs       
    _ -> Nothing


walkSList :: SymExeExpr -> [Value] -> Maybe SymExeExpr
walkSList sbv [] = return sbv
walkSList sbv (ix:ixs) = do
  intVal <- fromConstantInt ix
  let badC = S.null sbv .|| (S.length sbv .<= intVal)
      sbv' = unsafeCastSBV $ sbv S..!! intVal
  ite badC Nothing $ walkSList sbv' ixs
_ = Nothing

walkSList' :: SymExeExpr -> [Int] -> Maybe SymExeExpr
walkSList' sbv [] = return sbv
walkSList' sbv (ix:ixs) = do
  let badC = S.null sbv .|| (S.length sbv .<= intVal)
      intVal = fromIntegral ix
      sbv' = unsafeCastSBV $ sbv S..!! intVal
  ite badC Nothing $ walkSList' sbv' ixs
_ = Nothing

insertSList :: SymExeExpr -> [Int] -> SymExeExpr -> SymExeExpr
insertSList sbv [] slv = S.head sbv S..: slv
insertSList sbv [ix] slv = slv'
  where k = fromIntegral ix
        !slv' = S.take k slv S..++ (S.head sbv S..: S.drop (k+1) slv) 
insertSList sbv _ slv = slv

insertSList' :: SymExeExpr -> [Int] -> SymExeExpr -> SymExeExpr
insertSList' sbv [] slv = unsafeCastSBV sbv S..: slv
insertSList' sbv [ix] slv = slv'
  where k = fromIntegral ix
        !slv' = S.take k slv S..++ (unsafeCastSBV sbv S..: S.drop (k+1) slv) 
insertSList' sbv (ix:ixs) slv = 
   ite badC slv $ insertSList' sbv ixs slv'
  where badC = S.null slv .|| (S.length slv .<= intVal)
        intVal = fromIntegral ix
        slv' = unsafeCastSBV $ slv S..!! intVal
-}

----------------
mkValSbv :: IsValue a => a -> SymExecutor SymExeExpr
mkValSbv v =  
#ifdef ZYZ_DEBUG
    trace ("...(mkValSbv) making new symbolic variable for " ++ vSymName) . 
#endif   
    lift. fmap S.singleton $ free vSymName 
--    lift. fmap toSExpr $! mkSymSBV Nothing (kindOf $ toValue v) (Just vSymName ) 
  where  vSymName = printf ("(%s)\"%s\"") (show $ valueUniqueId v)
                           (fromMaybe "_" $ toVarName v)

mkValSbv_ :: IsValue a => a -> SymExecutor SymExeExpr
mkValSbv_ v =  
#ifdef ZYZ_DEBUG
    trace ("...(mkValSbv_) making free-name symbolic for " ++ toVarName' v) . 
#endif    
    lift. fmap S.singleton $ free_
--       lift. fmap toSExpr $! mkSymSBV Nothing (kindOf $ toValue v) Nothing 

mkExTypeSbv :: Type -> SymExecutor SymExeExpr
mkExTypeSbv t =  
#ifdef ZYZ_DEBUG
    trace ("...(mkExTypeSbv) making free-name symbolic for the type: " ++ show t) .
#endif     
    lift. fmap S.singleton $ free_
--      lift. fmap toSExpr $! mkSymSBV Nothing (kindOf t) Nothing    

newSExpr :: IsValue a => a -> SymExeInfo -> SymExecutor (SymExeExpr, SymExeInfo)
newSExpr v si = do
   let vName =  toVarName' v
   case lkpVar vName si of 
      Just sbv -> return (sbv,si) 
      Nothing  -> do
        sbv <- mkValSbv v
          -- lift. fmap S.singleton $ free vName   `debug` ("......(newSExpr) for " ++ vName)
        let si' = xtdInst (-1) sbv $! updVar vName sbv si
        return (sbv, si')

----     
instance HasKind Type where 
  kindOf  (TypeInteger n)  = KBounded True n
  kindOf  TypeFloat        = KFloat
  kindOf  TypeDouble       = KDouble
  kindOf  (TypeArray _ t)  = KList (kindOf t)
  kindOf  (TypeVector _ t) = KList (kindOf t)
  kindOf  TypeFP128        = KList (KBounded True 8)
  kindOf  TypeX86FP80      = KList (KBounded True 8)
  kindOf  TypePPCFP128     = KList (KBounded True 8)
  kindOf  TypeX86MMX       = KUninterpreted "TypeX86MMX" (Left "TypeX86MMX")
  kindOf  TypeVoid         = KTuple []
  kindOf  TypeLabel        = KUninterpreted "TypeLabel" (Left "TypeLabel")
  kindOf  TypeMetadata     = KUninterpreted "TypeMetadata" (Left "TypeMetadata")
  kindOf  (TypePointer t _)          = KTuple [kindOf t]
  kindOf  (TypeFunction rt ats _)    = KTuple $! kindOf rt : map kindOf ats
  kindOf  (TypeStruct (Left _) ts _)   = KTuple $! map kindOf ts  
  kindOf  (TypeStruct (Right _) _ _)  = KTuple [KBounded True 8] 

instance HasKind Value where 
  kindOf v = case (isUnsigned v, simpleValueType v) of
      (True, TypeInteger n)  ->  KBounded False n
      _  ->  kindOf $ simpleValueType v

-- instance SymVal Value where



---
getValuePTs :: IsValue a => a -> IndirectCallSummary -> [Value]
getValuePTs i ics = pointsTo (summaryTargets ics) (toValue i)

getGlbFldInits :: IsValue a => a -> Int -> IndirectCallSummary -> [Value]
getGlbFldInits base ix ics =  
  case HM.lookup (valueType base, ix) (globalInits ics) of
     Just glbInits -> HS.toList glbInits
     _             -> []


-----
insertVecVal :: Value -> Value -> Value -> [Value]
insertVecVal vec vi ix = fromMaybe [] $ do  
  k <- fromConstantInt ix
  let vs = getVecValues vec
      vs' = take k vs ++ vi : drop (k+1) vs
  if length vs <= k then Nothing
  else return vs'

getVecVal :: Value -> Value -> Maybe Value
getVecVal v ix = do
  intVal <- fromConstantInt ix
  let vs = getVecValues v
  if length vs <= intVal then Nothing
  else return $ vs !! intVal

insertAggVal :: Value -> Value -> [Int] -> [Value]
insertAggVal v _ [] = getAggValues v
insertAggVal v vi (k:ixs) = fromMaybe [] $ do  
  let vs = getAggValues v
      vs' = take k vs ++ vi : drop (k+1) vs
      vis = insertAggVal (vs !! k) vi ixs
      vs2 = if null vis then vs
            else take k vs ++ (head vis) : drop (k+1) vs
  case (length vs <= k, null ixs) of
    (True, _)      -> Nothing
    (False, True)  -> return vs'
    (False, False) -> return vs2
  

getAggVal :: Value -> [Int] -> Maybe Value
getAggVal v [] = return (stripBitcasts v)
getAggVal v (ix:ixs) = do
  let vs = getAggValues v
      v' = vs !! ix
  if length vs <= ix then Nothing
  else if null ixs then return v' 
       else getAggVal v' ixs

getVecValues :: Value -> [Value]
getVecValues v  = 
  case valueContent' (memAccessBase v) of
    ConstantC ConstantVector {constantVectorValues = vs} -> vs
    _ -> []

getAggValues :: Value -> [Value]
getAggValues v  = 
  case valueContent' (memAccessBase v) of
    ConstantC ConstantArray { constantArrayValues = vs } -> vs
    ConstantC ConstantStruct { constantStructValues = vs } -> vs
    _ -> []

getGepFinalVal :: Value -> [Value] -> Value
getGepFinalVal base ixs = fromMaybe bv $ 
  case (valueType base, ixs) of
    (TypePointer _ _, _:rest) -> resolveInitializer bv rest
    _ -> Nothing
  where  !bv = memAccessBase base
 
resolveInitializer :: Value -> [Value] -> Maybe Value
resolveInitializer v [] = return (stripBitcasts v)
resolveInitializer v (ix:ixs) = do
  intVal <- fromConstantInt ix
  case valueContent v of
    ConstantC ConstantArray { constantArrayValues = vs } ->
      if length vs <= intVal then Nothing else resolveInitializer (vs !! intVal) ixs
    ConstantC ConstantStruct { constantStructValues = vs } ->
      if length vs <= intVal then Nothing else resolveInitializer (vs !! intVal) ixs
    -- zyz
    -- ConstantC ConstantVector {constantVectorValues = vs} -> 
    --   if length vs <= intVal then Nothing else resolveInitializer (vs !! intVal) ixs
    _ -> Nothing

fromConstantInt :: (Num a, IsValue b) => b -> Maybe a
fromConstantInt v =
  case valueContent v of
    ConstantC ConstantInt { constantIntValue = iv } ->
      return $ fromIntegral iv
    _ -> Nothing


--------------
isArgOut :: OutputSummary -> Value -> Bool
isArgOut outSum v = fromMaybe False $ do
  arg <-  fromValue v
  let argInfo = getArgOutInfo outSum arg
      interop = [PAOut, PAInOut] `intersect` argInfo
  return . not $ null interop

getArgOutInfo :: OutputSummary -> Argument -> [ParamAnnotation]
getArgOutInfo outSum arg = 
    map fst $ summarizeArgument arg outSum



-----
getPtrValue :: Value -> Value
getPtrValue ptr = fromMaybe (memAccessBase ptr) $ do
   ap <- valueAsAccessPath ptr
   let absAP = abstractAccessPath ap
       baseV = accessPathBaseValue ap
   followAccessPath absAP baseV

accessPathBaseArgument :: AccessPath -> Maybe Argument
accessPathBaseArgument p =
  fromValue $ valueContent' (accessPathBaseValue p)

isLocalStoreAddr :: AccessPath -> Bool
isLocalStoreAddr acp = fromMaybe False $ do
  AllocaInst {} <- fromValue (accessPathBaseValue acp)
  return True
