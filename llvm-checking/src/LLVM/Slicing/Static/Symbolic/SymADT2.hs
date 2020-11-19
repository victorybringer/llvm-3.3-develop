{-# LANGUAGE BangPatterns,ViewPatterns,DeriveGeneric,NoMonomorphismRestriction,TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields -rtsopts #-}

module LLVM.Slicing.Static.Symbolic.SymADT2 where

import GHC.Generics ( Generic )
import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( makeLenses, (%~), (^.) )
import System.FilePath ( takeBaseName )
import System.Process (system)
import Control.Monad.RWS.Strict (RWS)
import qualified Control.Monad.RWS.Strict as RW

import Data.Map ( Map )
import qualified Data.Map as M
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM 
import Data.Set ( Set )
import qualified Data.Set as S
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.Maybe  
import Data.Monoid ( mempty )
import Data.List ( foldl' ) 
import Data.Hashable   -- ( hash )

import LLVM.Analysis

import LLVM.Slicing.Util.Utils
import LLVM.Slicing.Data.SliceType    
import qualified LLVM.Slicing.Data.BDD as BDD   

import qualified LLVM.Slicing.Data.Relation as R
import qualified Data.Graph.Inductive as G


----
type ValIdSet = BDD.ROBDD        --- ValueIds

data SliceEnv = SEnv {   --  procSliTbl :: !(IntMap SliceTable),  
                       procValueDep :: IntMap (IntMap ValIdSet)
                     , paraValMap :: !(IntMap (Value,Int))
                     , instCtrMap :: IntMap ValIdSet
                     }                      

data SliceState = SState { traces :: !Int         --  [Instruction]
                         } deriving (Generic)

data SliceInfo = SInfo { _valueDepMap :: IntMap ValIdSet 
                       }
              deriving (Eq,Ord,Show,Generic)

$(makeLenses ''SliceInfo)

instance NFData SliceState where
  rnf = genericRnf

instance NFData SliceInfo where
  rnf = genericRnf
  


----------------------------------------------
---
type Analysis = RWS SliceEnv String SliceState

runAnalysis :: RWS r b s a -> r -> s -> a 
runAnalysis a r s = fst $ RW.evalRWS a r s
analysisEnvironment = RW.asks
analysisGet = RW.get
analysisPut = RW.put
analysisLocal = RW.local
  

     
----------------------------------
------
unionLs :: IsValue a => IntMap ValIdSet -> SliceInfo -> Instruction -> a -> ValIdSet
unionLs cdM si i v = {-# SCC unionLs #-}  BDD.mrgBDD iID iDeps
  where iDeps = unionLkpValDep (si ^. valueDepMap) $! BDD.mrgBDD iCDs vDDs 
        iID = instructionUniqueId i  
        iCDs = IM.findWithDefault BDD.makeFalse iID cdM
        vDDs = IS.insert (valueUniqueId v) (instRefs v)
{-# INLINE unionLs #-}

--unionL' :: IsValue a => ValIdSet -> ValIdSet -> (a,SliceTable) -> ValIdSet
--unionL' l0 l (v,s) = {-# SCC unionL' #-}  BDD.mrgBDDs [l0, l, refIds, unionLkpSli refStrs s]
--  where !refValues = refVals v
--        !refIds = IS.fromList . HS.toList . HS.map valueUniqueId $ refValues 
--        !refStrs = mapMaybe toVarName . HS.toList $ refValues    
--{-# INLINE unionL' #-}


---
setValueDep :: IntMap ValIdSet -> SliceInfo -> SliceInfo
setValueDep idm = valueDepMap %~ mrgValDep idm

setTrValueDep :: IntMap ValIdSet -> SliceInfo -> Instruction -> Analysis SliceInfo  
setTrValueDep cdM si i = do
  addTrace i
  let si' = addValueDep si i $! unionLs cdM si i i
  return si'

addTrValueDep :: IsValue a => SliceInfo -> a -> ValIdSet -> Analysis SliceInfo
addTrValueDep si v l' = do
  addTrace'
  return $! addValueDep si v l'

addValueDep :: IsValue a => SliceInfo -> a -> ValIdSet -> SliceInfo
addValueDep si v l' =  updValueDep (valueUniqueId v) l' si

----
updValueDep :: UniqueId -> ValIdSet -> SliceInfo -> SliceInfo
updValueDep n ds = valueDepMap %~ IM.insert n ds    
{-# INLINE updValueDep #-}

updValueDep2 :: ValueIds -> ValIdSet -> SliceInfo -> SliceInfo
updValueDep2 ns ds si = 
    IS.foldl' (\si' n -> updValueDep n ds si') si ns   

xtdValueDep :: UniqueId -> ValIdSet -> SliceInfo -> SliceInfo
xtdValueDep n ds = valueDepMap %~ IM.insertWith BDD.mrgBDD n ds

xtdValueDep2 :: ValueIds -> ValIdSet -> SliceInfo -> SliceInfo
xtdValueDep2 ns ds si =  
    IS.foldl' (\si' n -> xtdValueDep n ds si') si ns 

updValDeps :: [(UniqueId,ValIdSet)] -> SliceInfo -> SliceInfo
updValDeps nds si = 
    foldl' (\si' (n,ds) -> updValueDep n ds si') si nds

xtdValDeps :: [(UniqueId,ValIdSet)] -> SliceInfo -> SliceInfo
xtdValDeps nds si = 
    foldl' (\si' (n,ds) -> xtdValueDep n ds si') si nds 

--
lkpValueDep ::  IsValue a => a -> IntMap ValIdSet -> ValIdSet
lkpValueDep v vdM = 
   IM.findWithDefault BDD.makeFalse (valueUniqueId v) vdM


-- data ValueSet = ValueSet Int ValIdSet        ( hash it)

--instance Hashable IntSet where
--  hashWithSalt s is =  s `hashWithSalt` 
--         (IS.size is) `hashWithSalt` (IS.toList is)
                        
mrgValDep :: IntMap ValIdSet -> IntMap ValIdSet -> IntMap ValIdSet
mrgValDep = {-# SCC mrgValDep #-} IM.unionWith BDD.mrgBDD 
--   where  hashUnion s1 s2 = if hash (IS.toList s1) == hash (IS.toList s2) 
--                            then s1 else IS.union s1 s2
--          {-# INLINE hashUnion #-}
{-# INLINE mrgValDep #-}

mrgValDepWith :: [UniqueId] -> IntMap ValIdSet -> IntMap ValIdSet -> IntMap ValIdSet
mrgValDepWith glbs vdM1 vdM2 = IM.unionWithKey mapF vdM1 vdM2  
  where mapF k lx' lx = if (elem k glbs) && lx' /= BDD.makeFalse && lx' /= kbdd 
                        then lx' else BDD.mrgBDD lx' lx
         where  kbdd = BDD.toBDD (-k) 
        {-# INLINE mapF #-}
{-# INLINE mrgValDepWith #-}

unionLkpValDep :: IntMap ValIdSet -> ValIdSet -> ValIdSet
unionLkpValDep vdm ns = {-# SCC unionLkpValDep #-}
  BDD.mrgBDDs. IM.elems $ IM.filterWithKey keyF vdm
  where  keyF k _ = BDD.elemBDD k ns
{-# INLINE unionLkpValDep #-}

unionLkpValDep' :: IntMap ValIdSet -> [UniqueId] -> ValIdSet
unionLkpValDep' vdM ns = case ns of 
  [] -> BDD.makeFalse
  [n] -> IM.findWithDefault BDD.makeFalse n vdM
  [n1,n2] -> BDD.mrgBDD nd1 nd2
     where nd1 = IM.findWithDefault BDD.makeFalse n1 vdM
           nd2 = IM.findWithDefault BDD.makeFalse n2 vdM
  _   -> unionLkpValDep vdM (BDD.toBDD ns)  
  

---
getTrace :: Analysis Int -- [Instruction]
getTrace = do {s <- analysisGet; return (traces s) }  

addTrace :: Instruction -> Analysis ()
addTrace i = addTrace'

addTrace' :: Analysis ()
addTrace' = do 
  s <- analysisGet  
  let tr' = 1 + traces s   
  analysisPut s { traces = tr'}



----------------------------------------------------------------------------
---
instDep2SrcDepRel :: IntMap Value -> IntMap ValIdSet -> R.Rel String String
instDep2SrcDepRel valMap idm = srcDepRel  
  where
    srcDepRel = S.unions [R.mkRelNeighbors a (S.toList bs)
                         | (a,bs) <- M.toList srcDepMap]
    srcDepMap = IM.foldlWithKey' doConvert mempty idm 
    doConvert acc i ls = 
      foldl' (\a v -> M.insertWith' S.union v (toSrcs ls) a) acc (toSrc i) 
    toSrc = maybeToList. valueSrc. findVal valMap
    toSrcs = S.fromList. mapMaybe valueSrc. findVals valMap

instDep2SrcLnDep :: IntMap Value -> IntMap ValIdSet -> Map String [String]
instDep2SrcLnDep valMap idm = M.map (toSrcLnStr valMap) srcLnDepMap  
  where
    srcLnDepMap = IM.foldlWithKey' doConvert mempty idm 
    doConvert acc i ls = 
      foldl' (\a v -> M.insertWith' BDD.mrgBDD v ls a) acc (toSrcLn i) 
    toSrcLn i = [fn ++ ":" ++ show ln | 
        (fn,ln) <- maybeToList (valueSrcLn $ findVal valMap i), not(null ln)]

--instDep2SrcLnDep2 :: IntMap Value -> IntMap ValIdSet -> IntMap ValIdSet
--instDep2SrcLnDep2 valMap idm = IM.foldlWithKey' doConvert mempty idm
--  where
--    doConvert acc i ls = 
--      foldl' (\a v -> IM.insertWith' IS.union v (toSrcLns ls) a) acc (toSrcLn i)    
--    toSrcLns = IS.fromList. concatMap valueLine . findVals valMap  
--    toSrcLn = valueLine . findVal valMap 

isValidInst v = case valueContent' v of 
   InstructionC RetInst {retInstValue = Nothing } -> False
   InstructionC RetInst {retInstValue = Just (valueContent -> ConstantC{})} -> False
   InstructionC UnconditionalBranchInst { } -> False 
   InstructionC UnreachableInst { } -> False
   InstructionC FenceInst { } -> False   
   InstructionC _  -> True
   _  -> False

showRelWith :: (Ord a, Show a) => (a -> String) -> R.Rel a a -> R.Rel a a -> String
showRelWith showNode ctr allr 
  = "digraph " ++ "inst_LDG" ++ " {\n" ++
    unlines nodes ++
    unlines dataEdges ++
    unlines ctrlEdges ++
    "}\n"
   where
      nodes = map (R.mkNode "". showNode) (S.elems $ R.ent allr)
      ctrlEdges = map showCEdge (S.elems ctr)
      dataEdges = map showDEdge (S.elems $ allr S.\\ ctr)
      showCEdge (x,y) = R.mkEdge "" (show x, show y)
      showDEdge (x,y) = R.mkEdge "color=\".8 .7 .8\" style=dashed" (show x, show y)  

-------------------------------------
reduceRel :: Ord a => R.Rel a a -> R.Rel a a
reduceRel r = r1 
  where suc r' a = S.fromList [ y | (x,y) <- S.elems r', x==a ] 
        pre r' a = S.fromList [ x | (x,y) <- S.elems r', y==a ]
        isNotTrans r' (x,y) = S.null $ S.intersection (suc r' x) (pre r' y)
        mapF r' a = if isNotTrans r' a then S.insert a r' else r'
        r1 = S.foldl' mapF S.empty (R.removeSelfEdges r)
        r2 = S.filter (isNotTrans r1) r1 
        
reduceRel2 r = r S.\\ (S.fromList $ concatMap tcFrom [ x | (x,_) <- S.elems r])
  where tcFrom x = [(x,z) | z <- S.elems zs]
          where ys = reachableFrom x
                zs = S.unions [S.intersection ys (reachableFrom y) | y <- S.elems ys]
        reachableFrom x  = R.rng (R.slice (S.singleton x) r)
                    
imis2r :: IntMap ValIdSet -> R.Rel Int Int
imis2r idm = S.unions [R.mkRelNeighbors a (IS.toList bs)
                      | (a,bs) <- IM.toList idm]

set2r :: (Ord b, Ord a) => Set (a,b) -> R.Rel a b
set2r = R.mkRel . S.toList

g2r :: G.Graph gr => gr a b -> R.Rel Int Int
g2r g = R.mkRel (G.edges g)

r2g :: R.Rel Int Int -> G.Gr () ()
r2g r = G.mkUGraph (S.toList $ R.ent r) (R.pairs r)

map2r :: (Ord b, Ord a) => Map a b -> R.Rel a b
map2r = R.mkRel . M.toList

sliceB vs r = R.sliceBackward (S.fromList vs) r
sliceF vs r = R.slice (S.fromList vs) r
chop2 vs1 vs2 r = R.chop (S.fromList vs1) (S.fromList vs2) r

--
printRelSli vars r r2 = printGr "Slice.dot" $ R.printRelSlice vars r r2

printInstRel fn = printGr fn. R.printGraphWith (show) (const $ R.quote . show)
                    "LDG_inst"
printSrcRel fn = printGr fn. R.printGraphWith (show) (const $ R.quote . show)
                    "LDG_srcln"                    
--printRel2 r1 r2 = printGr "LDGraph.dot" $ R.printRelTwo "LDG2" r1 r2


printGr' :: FilePath -> String -> IO ()
printGr' fn contents = do
   writeFile dotfile contents
   system $ "dot -Tpng " ++ dotfile ++ " -o " ++ pngfile
   return ()
  where 
   dotfile = if null fn then "Temp.LDGgraph.dot" else (takeBaseName fn ++ "_LDG.dot")
   pngfile = if null fn then "Temp.LDGgraph.png" else (takeBaseName fn ++ "_LDG.png")

      


    


