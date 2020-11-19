module LLVM.Slicing.Util.ValueMap where


import Control.Arrow
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Set ( Set )
import qualified Data.Set as S ( singleton,union )
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM ( fromList )
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.ByteString.Lazy.Char8 ( ByteString )
--import qualified Data.ByteString.Char8 as BS

import Data.Maybe 
import Data.Monoid ( mempty )
import Data.List 
import qualified Data.Foldable as F ( foldr )
--import System.IO.Unsafe ( unsafePerformIO )
import Data.Char ( isLetter )

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.CDG
import LLVM.Analysis.Dominance
import LLVM.Analysis.CallGraph

import LLVM.Slicing.Util.Mix
import LLVM.Slicing.Util.SrcLineNum ( valueLine,genSrcMap )
import LLVM.Slicing.Util.ValueTest  ( isCtrInst,isCallInst )




genValueMap :: Module -> IntMap Value
genValueMap m = {-# SCC genValueMap #-}  IM.fromList (zip allIds allVals)
  where   
    allVals = allValues m
    allIds = map valueUniqueId allVals
{-# INLINE genValueMap #-}


genAllParaMap :: Module -> IntMap (Value,Int)
genAllParaMap m = IM.fromList $ zip allParaIds allParaVals
  where 
    allParaIds = map (valueUniqueId . fst) allParaVals
    allParaVals = globalVals ++ concatMap frmlVals (moduleDefinedFunctions m)
    frmlVals fn = zip (map toValue $ functionParameters fn) [0..]
    -- allocVals fn = zip (map toValue $ funcAllocInsts fn) (repeat (-2))
    globalVals = zip glbValues (repeat (-1))
    glbValues = filter ((\(_:k:_) -> isLetter k || k == '_') . toVarName') $
         map toValue (moduleGlobalVariables m) ++ map toValue (moduleExternalValues m)

---
findVals :: IntMap Value -> IntSet -> [Value]
findVals m ks = IM.elems (IM.filterWithKey keyF m)
  where keyF k _ = IS.member k ks  

findVal = (^!)
(^!) :: IntMap Value -> UniqueId -> Value
m ^! k = IM.findWithDefault undef k m
  where undef = ConstantC UndefValue {constantType=TypeVoid, constantUniqueId = k}
  
id2Line :: Module -> Int -> [Int]
id2Line m k = toLine k
  where 
    valMap = genValueMap m
    toLine n = if n < 0 then [n] else valueLine (valMap ^! n) 
    
    
---

genSrcLineMap :: Module -> IntMap [Value]
genSrcLineMap m = IM.map (map (valMap ^!)) srcLnMap
  where 
    valMap = genValueMap m
    idLnMap = genIdLineMap' valMap
    srcLnMap = invIntMap idLnMap

genSrcLineMap' :: IntMap Value -> IntMap [Value]
genSrcLineMap' valMap = 
  IM.map (map (valMap ^!)) . invIntMap $ genIdLineMap' valMap  

genIdLineMap :: Module -> IntMap [Int]
genIdLineMap m = IM.map valueLine (genValueMap m)

genIdLineMap' :: IsValue a => IntMap a -> IntMap [Int]
genIdLineMap' valMap = IM.map valueLine valMap


---
genLineSrcMap :: FilePath -> IntMap ByteString
genLineSrcMap = genSrcMap

                   
------------------------------------------------------
--------    
genAliasMap :: Module -> Map String String
genAliasMap m = M.fromList $ glbAlias ++ concatMap getFunAlias fs
  where 
    glbAlias = [(show (globalAliasName ga), toVarName' (globalAliasTarget ga))
               | ga <- moduleAliases m ]
    fs = moduleDefinedFunctions m

genAliasMap1 :: Module -> IntMap Int
genAliasMap1 m = IM.fromList $ glbAlias ++ concatMap getFunAlias1 fs
  where 
    glbAlias = [(valueUniqueId ga, valueUniqueId $ globalAliasTarget ga)
               | ga <- moduleAliases m ]
    fs = moduleDefinedFunctions m
    
genAliasMap' :: Module -> HashMap Value Value
genAliasMap' m = HM.fromList $ glbAlias ++ concatMap getFunAlias' fs
  where 
    glbAlias = [(toValue ga, globalAliasTarget ga) | ga <- moduleAliases m]
    fs = moduleDefinedFunctions m 
    
-----
genCtrMap1 :: Function -> IntMap ValueIds
genCtrMap1 f = IM.map IS.fromList . invIntMap . IM.fromList . map (instructionUniqueId &&& 
       (map instructionUniqueId . controlDependencies cdg)) $ functionInstructions f
   where cdg = controlDependenceGraph $ controlFlowGraph f
       
genCtrMap2 :: HasCFG a => a -> IntMap ValueIds
genCtrMap2 = IM.map IS.fromList . invIntMap . IM.fromList . map (instructionUniqueId *** 
       (map instructionUniqueId)) . dominators . getDomTree . getCFG

genCtrMap :: HasCFG a => a -> IntMap IntSet        
genCtrMap = {-# SCC genCtrMap #-} 
    IM.fromList. map (instructionUniqueId ***(IS.fromList. map instructionUniqueId)). M.toList. 
    M.filterWithKey (\k _ -> isCtrInst k). foldr doInvert mempty. dominators . getDomTree . getCFG
  where 
    doInvert (k, vs) acc = foldr (\v a -> M.insertWith union v [k] a) acc vs 
{-# INLINE genCtrMap #-}

genInstCtrMap :: Module -> IntMap ValueIds
genInstCtrMap = IM.unions. map genCtrMap. moduleDefinedFunctions 


genFunCtrMap :: HasCFG a => a -> IntMap ValueIds        
genFunCtrMap = IM.fromList. map (instructionUniqueId ***(IS.fromList. map instructionUniqueId)). M.toList. 
    M.filterWithKey (\k _ -> isCtrInst k). foldr doInvert mempty. dominators . getDomTree . getCFG
  where 
    doInvert (k, vs) acc = foldr (\v a -> M.insertWith union v [k] a) acc vs 

genCtrDepMap :: Function -> IntMap ValueIds
genCtrDepMap f = IM.map IS.fromList . IM.fromList. map (instructionUniqueId &&& 
       (map instructionUniqueId . directControlDependencies cdg)) $ functionInstructions f
   where cdg = controlDependenceGraph $ controlFlowGraph f
{-# INLINE genCtrDepMap #-} 

genCtrDepMap2 :: Function -> IntMap ValueIds
genCtrDepMap2 f = IM.map IS.fromList . IM.fromList. map (instructionUniqueId &&& 
       (map instructionUniqueId . controlDependencies cdg)) $ functionInstructions f
   where cdg = controlDependenceGraph $ controlFlowGraph f
{-# INLINE genCtrDepMap2 #-}

genCallCtrMap :: CallGraph -> IntMap ValueIds
genCallCtrMap cg = IM.unions $ map (genFunCallCtrMap cg) allFuns  
  where  allFuns = callGraphFunctions cg

genFunCallCtrMap :: CallGraph -> Function -> IntMap ValueIds
genFunCallCtrMap cg f = IM.fromList allCallInfs
  where 
    allCallInfs = map (instructionUniqueId &&& getCallInfls) allCallInsts
    allCallInsts = filter isCallInst $ functionInstructions f
    getCallInfls = IS.fromList. map instructionUniqueId. concatMap functionInstructions.
       mapMaybe getFunc. concatMap (allFunctionCallees cg). mapMaybe getFunc. callSiteTargets cg         

genCallCtrMap2 :: Module -> IntMap ValueIds
genCallCtrMap2 m = IM.map (go IS.empty) callInfsMap
  where
     callInfsMap = IM.fromList allCallInfs
     allCallInfs = mapMaybe getCallInfls $ concatMap functionInstructions allFuns
     allFuns = moduleDefinedFunctions m
     getCallInfls i = case i of 
        CallInst {callFunction = fv} -> do 
          f <- getFunc fv
          let iID = instructionUniqueId i
              infls = map instructionUniqueId $ functionInstructions f
          return (iID, IS.fromList infls)
        InvokeInst {invokeFunction = fv} -> do 
          f <- getFunc fv
          let iID = instructionUniqueId i
              infls = map instructionUniqueId $ functionInstructions f
          return (iID, IS.fromList infls)
        _ ->  Nothing
    --
     go visited q
       | IS.null vals = visited
       | otherwise =
         let visited' = visited `IS.union` vals
             q' = foldl' addValuesFrom IS.empty (IS.toList vals)
         in go visited' q'
       where
         vals = IS.difference q visited 
         addValuesFrom :: ValueIds -> UniqueId -> ValueIds
         addValuesFrom q v = case IM.lookup v callInfsMap of
                  Just ls -> IS.union q ls
                  Nothing -> IS.insert v q
    

--------
invIntMap :: IntMap [Int] -> IntMap [Int]
invIntMap = foldl' doInvert IM.empty . IM.toList
  where
    doInvert acc (k, vs) =
      foldl' (\a v -> IM.insertWith union v [k] a) acc vs
      

invertMap' :: (Ord k, Ord v) => Map k (Set v) -> Map v (Set k)
invertMap' = foldr doInvert mempty . M.toList
  where
    doInvert (k, vset) acc =
      F.foldr (\v a -> M.insertWith S.union v (S.singleton k) a) acc vset

invertMap :: (Ord k, Ord v) => Map k [v] -> Map v [k]
invertMap = foldr doInvert mempty . M.toList
  where
    doInvert (k, vs) acc =
      foldr (\v a -> M.insertWith union v [k] a) acc vs  
