{-# LANGUAGE BangPatterns,ViewPatterns,NoMonomorphismRestriction #-}


module LLVM.Slicing.Util.DefRef where

import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM  
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS ( fromList )
--import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Data.Maybe 
import Data.List ( foldl',partition )
import Data.Tuple ( swap )
import Data.Hashable (Hashable)

import LLVM.Analysis

import LLVM.Slicing.Util.Mix
import LLVM.Slicing.Util.ValueTest
import LLVM.Slicing.Util.SrcLineNum   -- ( valueLine )



instDefs fms = hsToIS . valueDefs fms
instDDefs = hsToIS . directValueDefs
instRefs = IS.fromList . map valueUniqueId . directValueRefs
instRefs2 = hsToIS . valueRefs

defs :: IsValue a => a -> [String]
defs val =
  case valueContent' val of
    InstructionC StoreInst {storeAddress = ptr} -> refs ptr
    InstructionC AtomicRMWInst {atomicRMWPointer = ptr} -> refs ptr
    InstructionC AtomicCmpXchgInst {atomicCmpXchgPointer = ptr} -> refs ptr
--    InstructionC CallInst {invokeFunction = cf, invokeArguments = (map fst -> args)} -> 
--              if isMemcpy cf || isMemset cf then refs (args!!0) else ...
    _ -> [] 

defVars :: IsValue a => a -> [Value]
defVars = HS.toList . HS.filter (isJust . toVarName). valueDefs mempty

defVals :: IsValue a => a -> HashSet Value
defVals = HS.filter (not . isConstant). valueDefs mempty


valueDefs :: IsValue a => IntMap (HashSet Value) -> a -> HashSet Value
valueDefs fms iv = HS.delete (toValue iv). recursiveOp addValuesFrom. HS.singleton $ toValue iv  
 where 
  insert v q = if isConstant v then q else HS.insert v q
  addValuesFrom :: HashSet Value -> Value -> HashSet Value
  addValuesFrom q v =
   case valueContent' v of        
    InstructionC BranchInst {} -> q
    InstructionC SwitchInst {} -> q
    InstructionC IndirectBranchInst {} -> q
    InstructionC UnconditionalBranchInst { } -> q
    InstructionC RetInst {} -> q
    
    InstructionC StoreInst {storeAddress = ptr} -> insert (memAccessBase ptr) q        
    InstructionC AtomicRMWInst {atomicRMWPointer = ptr} -> insert (memAccessBase ptr) q
    InstructionC AtomicCmpXchgInst {atomicCmpXchgPointer = ptr} -> insert (memAccessBase ptr) q
    InstructionC InsertValueInst { insertValueAggregate = a} -> insert a q
    
    InstructionC CallInst {callFunction = cf, callArguments = (map fst -> args)} ->
         getCallInstDefs fms cf args q
    InstructionC InvokeInst {invokeFunction = cf, invokeArguments = (map fst -> args)} ->
         getCallInstDefs fms cf args q
--    InstructionC i -> case instructionFunction i of
--                Nothing -> q
--                Just iFun -> if i /= functionEntryInstruction iFun then q
--                             else foldl' (flip HS.insert) q (map toValue $ functionParameters iFun) 
    FunctionC f -> HS.union q mods 
         where  mods = IM.findWithDefault HS.empty (functionUniqueId f) fms   -- computeModSet f                
    _ -> q   -- insert (stripBitcasts v) q     
    
  getCallInstDefs :: IntMap (HashSet Value) -> Value -> [Value] -> HashSet Value -> HashSet Value
  getCallInstDefs fms fv args q = 
    case valueContent' fv of
      FunctionC f -> HS.union q mods  
        where  fID = functionUniqueId f 
               fFmls = map toValue $ functionParameters f
               mods = HS.union (HS.fromList . map fst $ argMap') 
                      (fMods `HS.difference` (HS.fromList . map snd $ argMap'))
               fMods = IM.findWithDefault HS.empty fID fms
               argMap = zip args fFmls 
               argMap' = filter (flip HS.member fMods . snd) argMap
      ExternalFunctionC ef -> if (isMemCMS ef || isC99Read ef)
                              then insert (memAccessBase $ args !! 0) q
                              else if isC99Scanf ef 
                                   then insert (memAccessBase $ args !! 1) q 
                                   else q
      _ -> q         
{-# INLINE valueDefs #-} 

--
directValueDefs = valueDefs2
valueDefs2 :: IsValue a => a -> HashSet Value
valueDefs2 iv = HS.delete (toValue iv). HS.filter (not . isConstant). 
                recursiveOp addValuesFrom. HS.singleton $ toValue iv  
 where 
  addValuesFrom :: HashSet Value -> Value -> HashSet Value
  addValuesFrom q v =
   case valueContent' v of        
    InstructionC BranchInst {} -> q
    InstructionC SwitchInst {} -> q
    InstructionC IndirectBranchInst {} -> q
    InstructionC UnconditionalBranchInst { } -> q
    InstructionC RetInst {} -> q
    
    InstructionC StoreInst {storeAddress = ptr} -> HS.insert (memAccessBase ptr) q        
    InstructionC AtomicRMWInst {atomicRMWPointer = ptr} -> HS.insert (memAccessBase ptr) q
    InstructionC AtomicCmpXchgInst {atomicCmpXchgPointer = ptr} -> HS.insert (memAccessBase ptr) q
    InstructionC InsertValueInst { insertValueAggregate = a} -> HS.insert a q
    
    InstructionC CallInst {callFunction = cf, callArguments = (map fst -> args)}          
      | isMemCMS cf   -> HS.insert (memAccessBase $ args !! 0) q
      | isC99Scanf cf -> HS.insert (memAccessBase $ args !! 1) q
      | isC99Read cf  -> HS.insert (memAccessBase $ args !! 0) q
      | otherwise -> q  --  HS.insert cf q   
    InstructionC InvokeInst {invokeFunction = cf, invokeArguments = (map fst -> args)}           
      | isMemCMS cf   -> HS.insert (memAccessBase $ args !! 0) q
      | isC99Scanf cf -> HS.insert (memAccessBase $ args !! 1) q
      | isC99Read cf  -> HS.insert (memAccessBase $ args !! 0) q
      | otherwise -> q  --  HS.insert cf q   
--    FunctionC f -> HS.union funDDefs q
--      where  funDDefs = HS.filter (not . isLocalToFunction f). HS.unions . map valueDefs2 $ functionInstructions f
    _ -> q   -- HS.insert (stripBitcasts v) q          
{-# INLINE valueDefs2 #-} 

-----------
refs :: IsValue a => a -> [String]
refs = mapMaybe toVarName . HS.toList . refVals    -- HS.toList . valueRefs . toValue 
{-# INLINE refs #-}

refVars :: IsValue a => a -> [Value]
refVars = HS.toList . HS.filter (isJust . toVarName). valueRefs

refVals :: IsValue a => a -> HashSet Value
refVals = HS.filter (not . isConstant). valueRefs
{-# INLINE refVals #-}

valueRefs :: IsValue a => a -> HashSet Value
valueRefs = {-# SCC valueRefs #-}  
   recursiveOp addValuesFrom. HS.singleton . toValue 
  where 
    addValuesFrom :: HashSet Value -> Value -> HashSet Value
    addValuesFrom q v =
      case valueContent' v of        
        InstructionC StoreInst {storeValue = sv} -> HS.insert sv q
        InstructionC LoadInst {loadAddress = la} -> HS.insert (memAccessBase la) q
        InstructionC i@PhiNode {} -> foldl' (flip HS.insert) q vs
          where !vs = instructionOperands i ++ (maybeToList $ phi2Var v)
        InstructionC CallInst {callFunction = cf, callArguments = (map fst -> args)} ->
          if (isMemcpy cf || isMemmove cf)&& (length args >= 2) then 
             HS.insert (memAccessBase $ args !! 0) $ HS.insert (memAccessBase $ args !! 1) q
          else if isMemset cf then HS.insert (memAccessBase $ args !! 0) q
               else foldl' (flip HS.insert) q (args ++ returnValue cf)   -- (cf : args)
        InstructionC InvokeInst {invokeFunction = cf, invokeArguments = (map fst -> args)} ->
          if (isMemcpy cf || isMemmove cf) && (length args >= 2) then 
             HS.insert (memAccessBase $ args !! 0) $ HS.insert (memAccessBase $ args !! 1) q
          else if isMemset cf then HS.insert (memAccessBase $ args !! 0) q
               else foldl' (flip HS.insert) q (args ++ returnValue cf)   -- cf : args
        InstructionC i -> foldl' (flip HS.insert) q (instructionOperands i)
        FunctionC f -> foldl' (flip HS.insert) q (maybeToList $  funcExitValue f)  -- HS.union funRefs q        
--          where  funRefs = HS.filter (not . isLocalToFunction f). HS.unions . 
--                              map valueRefs $ functionInstructions f
        _ -> HS.insert (stripBitcasts v) q             -- HS.insert v $    -- ignoreCasts
{-# INLINE valueRefs #-} 

valueRefs1 iv = HS.delete (toValue iv). HS.filter (not. isConstant) $ valueRefs iv

---
valueRefs2 :: IsValue a => a -> HashSet Value
valueRefs2 = HS.fromList . directValueRefs

directValueRefs :: IsValue a => a -> [Value]
directValueRefs iv = {-# SCC directValueRefs #-} filter (not. isConstant) $! 
  let v = toValue iv in
  case valueContent' v of        
    InstructionC StoreInst {storeValue = sv} -> sv : returnValue sv
    InstructionC LoadInst {loadAddress = la} -> [memAccessBase la]
--    InstructionC i@PhiNode {} -> instructionOperands i ++ (maybeToList $ phi2Var v)
    InstructionC CallInst {callFunction = cf, callArguments = (map fst -> args)} ->
      if (isMemcpy cf || isMemmove cf) && (length args >= 2) then 
         [memAccessBase $ args !! 0,memAccessBase $ args !! 1]
      else if isMemset cf then [memAccessBase $ args !! 0]
           else args ++ returnValue cf
    InstructionC InvokeInst {invokeFunction = cf, invokeArguments = (map fst -> args)} ->
      if (isMemcpy cf || isMemmove cf)&& (length args >= 2) then 
         [memAccessBase $ args !! 0,memAccessBase $ args !! 1]
      else if isMemset cf then [memAccessBase $ args !! 0]
           else args ++ returnValue cf
    InstructionC i -> instructionOperands i
    FunctionC f -> maybeToList $ funcExitValue f  
    _ -> [stripBitcasts v]            
{-# INLINE directValueRefs #-}


refs2 :: IsValue a => a -> [String]
refs2 = mapMaybe toVarName2 . HS.toList . refVals    
{-# INLINE refs2 #-}

-- | Consider the variable name in a Store instruction for forward slicing
toVarName2 :: Value -> Maybe String
toVarName2 val =
  case valueContent' val of
    InstructionC i@StoreInst {storeAddress = ptr} -> Just ptrName    
       where lnInfo = case valueLine i of
                      [m] -> ":" ++ show m
                      _ -> ""
             ptrName = toVarName' ptr ++ ":" ++ show (valueLine i)  
    _  -> toVarName val  
{-# INLINE toVarName2 #-}

recursiveOp :: (Eq a, Hashable a) => (HashSet a -> a -> HashSet a) -> HashSet a -> HashSet a
recursiveOp addValuesFrom = {-# SCC recursiveOp #-} go HS.empty   -- . HS.singleton   
  where 
    go visited q
      | HS.null vals = visited
      | otherwise =
        let visited' = visited `HS.union` vals
            q' = foldl' addValuesFrom HS.empty (HS.toList vals)
        in go visited' q'
      where
        vals = HS.difference q visited
{-# INLINE recursiveOp #-}

---- 
inValRefs :: IsValue a => Value -> a -> Bool
inValRefs target v = target == memAccessBase (toValue v) 
                    || HS.member target (valueRefs v)

isVarRefs :: IsValue a => Value -> a -> Bool      
isVarRefs target v = toVarName' target `elem` refs v

---------------
----
outputVarRefs :: IsValue a => a -> [Value]
outputVarRefs iv = filter (not. isConstant) $! 
  case valueContent' iv of 
    InstructionC CallInst {callFunction = cf, callArguments = (map fst -> args)} 
      | isC99Printf cf  -> map memAccessBase (drop 1 args)
      | isC99Output cf  -> map memAccessBase (take 1 args)
      | otherwise -> []
    InstructionC InvokeInst {invokeFunction = cf, invokeArguments = (map fst -> args)} 
      | isC99Printf cf  -> map memAccessBase (drop 1 args)
      | isC99Output cf  -> map memAccessBase (take 1 args)
      | otherwise -> []
    InstructionC RetInst {retInstValue = Just rv} -> [memAccessBase rv]
    _ -> [] 

funcOutputVars :: Function -> HashSet Value
funcOutputVars f = HS.fromList $ filter isValidVar allOutVars2
  where 
    instOutVars =  concatMap outputVarRefs (functionInstructions f)
    (phiVars,nonPhiVars) = partition isPhiNode instOutVars
    allOutVars = nonPhiVars ++ mapMaybe phi2Var phiVars
    (tmpVars,nonTmpVars) = partition isTempVar allOutVars
    allOutVars2 = nonTmpVars ++ mapMaybe (flip HM.lookup fAliasMap) tmpVars
    fAliasMap = HM.fromList. map swap $ getFunAlias' f
    

---------------------------------------------        
-------
computeModSet :: [Function] -> IntMap (HashSet Value)
computeModSet = go IM.empty 
  where     
    fAliasMap = HM.fromList. map swap . getFunAlias'
    addFMods fms fs = foldl' (\fms' f -> addFunMod (fAliasMap f) fms' f) fms fs
    go fms funs = if fms' == fms then fms else go fms' funs
          where  fms' = addFMods fms funs  

addFunMod :: HashMap Value Value -> IntMap (HashSet Value) -> Function -> IntMap (HashSet Value)
addFunMod fAliasMap fms f = fms'
  where 
    fms' = IM.insertWith HS.union (functionUniqueId f) fMod fms
    fMod = getFunDefsOrRefs fAliasMap (valueDefs fms) f

getFunDefsOrRefs :: HashMap Value Value -> (Instruction -> HashSet Value) -> Function -> HashSet Value
getFunDefsOrRefs fAliasMap defOrRef f = fDefsOrRefs
  where 
    fDefsOrRefs = addAlias. HS.filter fitF. HS.unions. map defOrRef $ fInsts 
    addAlias :: HashSet Value -> HashSet Value        
    addAlias vs = HS.union (HS.fromList. HM.elems $ HM.filterWithKey mapF fAliasMap) vs
          where mapF k _ = HS.member k vs
--    fAliasMap = HM.fromList. map swap $ getFunAlias' f
    fInsts = functionInstructions f
    fitF v = if hasExtraReference v then not (isLocalToFunction f v)
             else not (isLocalToFunction f v || isConstant v)
             
----
functionRefs :: HashMap Value Value -> Function -> HashSet Value
functionRefs fAliasMap = HS.filter isValidVar2 . getFunDefsOrRefs fAliasMap valueRefs
  where  isValidVar2 v = case valueContent v of  
             ArgumentC _ -> True 
             GlobalVariableC _ -> True
             ExternalValueC _ -> True
             _  -> False

functionVars :: Function -> [String]
functionVars f = fmlVars ++ locVars
  where fmlVars = map toVarName' $ functionParameters f 
        locVars = map toVarName' $ funcAllocInsts f
        

functionRets :: Function -> [Value]
functionRets f = concatMap fromReturn exitInsts
  where
    exitInsts = functionExitInstructions f
    fromReturn i =
      case i of
        RetInst { retInstValue = Just rv } ->
          case valueContent' rv of
            InstructionC PhiNode {} -> filter (/= rv) $ flattenValue rv
            _ -> [rv]
        _ -> []

--
isEnumVar = isVarWith "enum"
isUnsignedVar = isVarWith "unsigned"
isConstVar = isVarWith "const"
getEnumDef = getDefSrcWith "enum"
getUnsignedDef = getDefSrcWith "unsigned"
getConstDef = getDefSrcWith "const"

isVarWith ty v = if null (getDefSrcWith ty v) then False else True

getDefSrcWith :: IsValue v => String -> v -> [String]
getDefSrcWith ty v =
  case (null vars, null srcs) of 
    (False,False) -> map BS.unpack srcs
    _  -> [] 
  where 
    vars = refs v ++ defs v
    varName va = takeWhile (/= '@') $ drop 1 va
    srcs' = getValueSrc2' v
    srcs = filter filtF srcs'
    filtF s = BS.isPrefixOf (BS.pack ty) s && isIns vars s
    isIns vas s = or $ map (isIn s) vas
    isIn s va  = BS.isInfixOf (BS.pack $ " " ++ varName va) s  ||
                 BS.isInfixOf (BS.pack $ "*" ++ varName va) s
    

     





---
flattenValue :: Value -> [Value]
flattenValue = HS.toList . recursiveOp addValuesFrom . HS.singleton
  where    
    addValuesFrom :: HashSet Value -> Value -> HashSet Value
    addValuesFrom q v =
      case valueContent' v of
        InstructionC PhiNode { phiIncomingValues = pvs } ->
          let vs = map fst pvs
          in foldr HS.insert q vs
        InstructionC SelectInst { selectTrueValue = tv, selectFalseValue = fv } ->
          foldr HS.insert q [tv, fv]
        _ -> HS.insert v $ HS.insert (stripBitcasts v) q