{-# LANGUAGE NoMonomorphismRestriction #-}

module LLVM.Slicing.Util.Mix where


import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS ( fromList )
import Data.Maybe 
import Data.List 
import Data.Char ( isDigit,toLower,isSpace )
import qualified Data.Text as T (unpack)
import System.FilePath (takeBaseName)

import qualified ABI.Itanium as ABI

import LLVM.Analysis
import LLVM.Analysis.CFG 
import LLVM.Analysis.CDG
import LLVM.Analysis.Util.Names
import LLVM.Analysis.BlockReturnValue
import LLVM.Analysis.Dominance
import LLVM.Analysis.AccessPath

import LLVM.Slicing.Util.ValueTest  -- ( isConstant,isAllocaInst,isTempVar )


--------

type InstHashSet = HashSet Instruction
type ValueIds = IntSet
type ValueId = Int

hsToIS :: IsValue a => HashSet a -> ValueIds
hsToIS = IS.fromList . HS.toList . HS.map valueUniqueId 

-----------------
instance HasPostdomTree Function where
  getPostdomTree = postdominatorTree . controlFlowGraph

instance HasCDG Function where
  getCDG = controlDependenceGraph . controlFlowGraph

instance HasDomTree Function where
  getDomTree = dominatorTree . controlFlowGraph
  
instance HasBlockReturns Function where
  getBlockReturns = labelBlockReturns

instRet :: Instruction -> Maybe Value
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


valueAsAccessPath :: Value -> Maybe AccessPath
valueAsAccessPath v = fromValue v >>= accessPath


getAPfinalTag :: Value -> Maybe (Type, AccessType)
getAPfinalTag v = do
  ap <- valueAsAccessPath v
  let apPaths = accessPathTaggedComponents ap
  return $ last apPaths 

--
returnValue :: IsValue a => a -> [Value]
returnValue fv = maybeToList $ do
   f <- getFunc (toValue fv) 
   rv <- funcExitValue f
   return rv
     
funcExitValue :: Function -> Maybe Value
funcExitValue f = 
   case functionExitInstruction f of
      Just i@RetInst {retInstValue = Just rv} -> 
         if isConstant rv then Nothing else Just rv
      _ -> Nothing

getFunc :: Value -> Maybe Function
getFunc v = case valueContent v of   
       FunctionC f -> Just f
       _  -> Nothing  

getFuncName,getFunctionName :: Function -> String
getFuncName = functionToDemangledName   --  getFunctionName


getFuncName' :: Function -> String
getFuncName' = identifierAsString . functionName     -- show

getExFuncName' = identifierAsString. externalFunctionName

getFunctionName = parseCppName . getFuncName'
getExFunctionName = parseCppName . getExFuncName'

parseCppName :: String -> String
parseCppName fname = 
  case ABI.demangleName fname of
    Left e -> fname
    Right dn@(ABI.Function sname _) -> parseName dn sname
    Right dn@(ABI.OverrideThunk _ (ABI.Function sname _)) -> parseName dn sname 
    Right n ->  ABI.cxxNameToString n
  where
    parseName dn sn = fromMaybe (ABI.cxxNameToString dn) (unparseFunctionName sn)
{-# INLINE parseCppName #-} 

getValueName :: IsValue a => a -> Maybe String
getValueName v = do
  vname <- valueName (toValue v)
  let vname' = identifierAsString vname
  case ABI.demangleName vname' of
    Left e -> return vname'
    Right n -> return $! ABI.cxxNameToString n


fun2DName = functionToDemangledName
functionToDemangledName :: Function -> String
functionToDemangledName f =
  case parseFunctionName f of
    Left e -> fname   -- "!!ERROR: " ++ e
    Right sname ->
      case unparseFunctionName sname of
        Nothing -> fname 
        Just n -> n
  where
    fname = getFuncName' f

getModuleName :: Module -> String
getModuleName = takeBaseName . T.unpack . moduleIdentifier

--
phi2Var :: IsValue a => a -> Maybe Value
phi2Var pv = {-# SCC phi2Var #-}  case valueContent' (toValue pv) of 
   InstructionC PhiNode {phiIncomingValues = pvs} ->
     case partition (isConstant . fst) pvs of 
       ([],[]) -> Nothing
       (vs,[]) -> listToMaybe2 $ mapMaybe getVar vs
       (_, vs) -> listToMaybe2 $ mapMaybe getVar vs 
   _ -> Nothing
   where
      getVar (val,lab) = let ls = lkpVars val lab in
        if not (null ls) then listToMaybe2 ls
        else case valueContent' val of
          InstructionC LoadInst {loadAddress = la} -> Just la
          _ -> Nothing 
      lkpVars v = mapMaybe (matchVar v). basicBlockInstructions. toBB 
      listToMaybe2 ls = case nub ls of
          [v] -> Just v   
          _   -> Nothing  
      matchVar v i = case i of 
        StoreInst {storeValue = sv, storeAddress = sa} ->
           if sv == v then Just sa else Nothing
        _ -> Nothing
      toBB v = case valueContent v of
        BasicBlockC bb -> bb
        _ -> error ("Expected basic block: " ++ show v)
{-# INLINE phi2Var #-}        

---
getAccessType v = fromMaybe (valueType $ memAccessBase v) (getAccessType' v)

getAccessType' :: Value -> Maybe Type
getAccessType' v = do
  i <- fromValue v
  ap <- accessPath i
  let vty = accessPathEndType ap 
  return vty

simpleValueType :: IsValue v => v -> Type
simpleValueType = simpleType . valueType . memAccessBase . toValue

simpleType ty = 
  case ty of 
    TypePointer ty' _ -> simpleType ty'
    TypeArray _ ty' -> simpleType ty'
    TypeFunction ty' _ _ -> simpleType ty'
    TypeVector _ ty' -> simpleType ty'
    _   -> ty


freePointerType ty = 
  case ty of 
    TypePointer ty' _ -> freePointerType ty'
    _   -> ty

typeSize :: Type -> Int
typeSize ty = 
  case ty of 
    TypePointer t _ -> typeSize t
    TypeArray n _ -> n
    TypeVector n _ -> n
    TypeStruct _ ts _ -> length ts
    _   -> 1

valueSize, simpleSize :: IsValue a => Module -> a -> Int
valueSize m = fromMaybe 1 . moduleTypeSizes m . 
       freePointerType . valueType . memAccessBase . toValue

simpleSize m = fromMaybe 1 . moduleTypeSizes m . simpleValueType

---- 
vEq = valueEq
valueEq :: (IsValue a,Eq a) => a -> a -> Bool
valueEq v1 v2 = (v1 == v2) || (toVarName' v1 == toVarName' v2) 
        
toValName, toVarName' :: IsValue a => a -> String
toValName v = maybe (valStr v) show (valueName v)
  where valStr v = "_" ++ show (valueUniqueId v)

toVarName' val = fromMaybe (toValName val) $ toVarName val
{-# INLINE toVarName' #-}
 
toVarName :: IsValue a => a -> Maybe String 
toVarName val =
  case valueContent' (toValue val) of
    GlobalVariableC gv -> Just $ show (globalVariableName gv)
    FunctionC f -> Just $ showFuncName f
    ExternalFunctionC ef -> Just $ "@" ++ getExFunctionName ef  -- getExFuncName'
    ArgumentC av -> Just $ argName av
    GlobalAliasC ga -> Just $ show (globalAliasName ga)   
    ExternalValueC ev  -> Just $ show (externalValueName ev)
    InstructionC i@PhiNode {} -> do {v <- phi2Var i; toVarName v} 
    InstructionC i -> Just $ instName i    
    _ -> fmap show (valueName val)     -- Nothing   
  where  
    instName i = maybe (instIdStr i) show (instructionName i) ++ 
                 maybe "@**" showFuncName (instructionFunction i)
    instIdStr i = "_" ++ show (instructionUniqueId i)
    argName av = show (argumentName av) ++ (showFuncName $ argumentFunction av)
    showFuncName f = "@" ++ getFuncName f
{-# INLINE toVarName #-} 


--
getModPtr :: Instruction -> Maybe Value
getModPtr StoreInst {storeAddress = ptr} = Just $ memAccessBase ptr
getModPtr AtomicRMWInst {atomicRMWPointer = ptr} = Just $ memAccessBase ptr
getModPtr AtomicCmpXchgInst {atomicCmpXchgPointer = ptr} = Just $ memAccessBase ptr
getModPtr InsertValueInst { insertValueAggregate = a} = Just a
getModPtr _  = Nothing

---
getFunAlias :: Function -> [(String,String)]
getFunAlias = mapBoth toVarName' toVarName'. getFunAlias'

getFunAlias1 :: Function -> [(Int,Int)]
getFunAlias1 = mapBoth valueUniqueId valueUniqueId . getFunAlias'

getFunAlias' :: Function -> [(Value,Value)]
getFunAlias' = mapMaybe getInstAlias. functionInstructions
  where
    getInstAlias :: Instruction -> Maybe (Value,Value)
    getInstAlias i = do
      (ptr,sv) <- getStoreInstInfo i
      argVal <- getArgVal sv
      ptrVal <- getAllocVal ptr
      return (argVal, ptrVal)
    --    
    getStoreInstInfo i = case i of 
      StoreInst {storeAddress = ptr, storeValue = sv} -> Just (ptr,sv)
      _  -> Nothing     
    getArgVal v = case valueContent' v of 
      GlobalVariableC _ -> Just v 
      GlobalAliasC _ -> Just v
      ArgumentC av -> if elem av (functionParameters $ argumentFunction av) 
                      then Just v     
                      else Nothing
      _  -> Nothing
    getAllocVal v = case valueContent' v of 
      InstructionC i@AllocaInst {} -> if isTempVar i then Just v else Nothing
--         where  isTempVar = maybe False (isDigit. head. drop 1. show). valueName
      _ -> Nothing   
{-# INLINE getFunAlias' #-} 
----------------------------
------
getInst :: Value -> Maybe Instruction
getInst = fromValue

funcAllocInsts :: Function -> [Instruction]
funcAllocInsts f = allocInsts
  where
--    allVars = zip allocVars (repeat $ identifierAsString (functionName f))
    allocInsts = filter isAllocaInst $ functionInstructions f
--    allocVars =  map identifierAsString $ mapMaybe instructionName allocInsts  


funTopInsts :: Function -> ValueIds
funTopInsts f = IS.fromList. map instructionUniqueId. concatMap basicBlockInstructions $ 
                (functionBody f) \\ (concatMap getInnerBBs $ functionInstructions f)
 where
   getInnerBBs i =  case i of
     BranchInst {branchTrueTarget = tb, branchFalseTarget = fb} -> [tb,fb]
     SwitchInst {switchDefaultTarget = db, switchCases = cases} -> db : map snd cases
     IndirectBranchInst {indirectBranchTargets = bs} -> bs
     _ -> []

--
succInst = instructionSuccessor
instructionSuccessor :: Instruction -> Maybe Instruction
instructionSuccessor i =
  case rest of
    _:nxt:_ -> Just nxt
    _ -> Nothing
  where
    Just bb = instructionBasicBlock i
    rest = dropWhile (/=i) (basicBlockInstructions bb)

preInst = instructionPredecessor
instructionPredecessor :: Instruction -> Maybe Instruction
instructionPredecessor i =
  if null rest then Nothing else Just (last rest)
  where
    Just bb = instructionBasicBlock i
    rest = takeWhile (/=i) (basicBlockInstructions bb)

instSuccs = instructionSuccessors
instructionSuccessors :: CFG -> Instruction -> [Instruction]
instructionSuccessors cfg i =
  if i == ti 
  then map blockEntryInst bs 
  else [rest !! 1]
  where
    Just bb = instructionBasicBlock i
    ti = basicBlockTerminatorInstruction bb
    bs = basicBlockSuccessors cfg bb
    rest = dropWhile (/=i) (basicBlockInstructions bb)
    blockEntryInst = head . basicBlockInstructions

instructionPredecessors :: CFG -> Instruction -> [Instruction]
instructionPredecessors cfg i =
  if instructionIsEntry i  
  then map basicBlockTerminatorInstruction ps 
  else [last rest]
  where
    Just bb = instructionBasicBlock i
    ps = basicBlockPredecessors cfg bb
    rest = takeWhile (/=i) (basicBlockInstructions bb)

----
--

mapFsts :: (a -> b) -> [(a,c)] -> [(b,c)]
mapFsts f = mapBoth f id

mapSnds :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnds g = mapBoth id g

mapBoth :: (a -> a') -> (b -> b') -> [(a,b)] -> [(a',b')]
mapBoth f g xs = [(f x, g y) | (x,y) <- xs]

sortFst :: Ord a => [(a,b)] -> [(a,b)]
sortFst = sortBy (\(x,_) (y,_) -> compare x y)

groupFst :: Eq a => [(a, t)] -> [[(a, t)]]
groupFst = groupBy (\(x,_) (y,_) -> x == y) 


             
---------------------------------------------------------------------
--- Citing from LLVM.Analysis
memAccessBase :: Value -> Value
memAccessBase ptr =
  case valueContent' ptr of
    GlobalVariableC gv -> toValue gv
    InstructionC i@AllocaInst {} -> toValue i
    ArgumentC a -> toValue a
    InstructionC LoadInst { loadAddress = la } -> memAccessBase la  -- stripBitcasts
    InstructionC GetElementPtrInst { getElementPtrValue = base } ->
      memAccessBase base
    InstructionC PhiNode {} -> fromMaybe ptr (phi2Var ptr)          
    _ -> stripBitcasts ptr

allValues :: Module -> [Value]
allValues m = allVals
  where
    fs = moduleDefinedFunctions m
    allArgs = concatMap functionParameters fs
    allBlocks = concatMap functionBody fs
    allInsts = concatMap basicBlockInstructions allBlocks
    allVals = concat [ map toValue fs
                     , map toValue (moduleGlobalVariables m)
                     , map toValue (moduleExternalValues m)
                     , map toValue (moduleExternalFunctions m)
                     , map toValue (moduleAliases m)
                     , map toValue allBlocks
                     , map toValue allInsts
                     , map toValue allArgs
                     ]

ignoreCasts :: Value -> Value
ignoreCasts v =
  case valueContent v of
    InstructionC BitcastInst { castedValue = cv } -> ignoreCasts cv
    InstructionC TruncInst { castedValue = cv } -> ignoreCasts cv
    InstructionC ZExtInst { castedValue = cv } -> ignoreCasts cv
    InstructionC SExtInst { castedValue = cv } -> ignoreCasts cv
    InstructionC IntToPtrInst { castedValue = cv } -> ignoreCasts cv
    GlobalAliasC GlobalAlias { globalAliasTarget = t } -> ignoreCasts t
    ConstantC ConstantValue { constantInstruction = BitcastInst { castedValue = cv } } -> ignoreCasts cv
    _ -> valueContent v

