{-# LANGUAGE BangPatterns,ViewPatterns,DeriveGeneric,TemplateHaskell,NoMonomorphismRestriction,
             RankNTypes,FlexibleContexts,FlexibleInstances,DeriveAnyClass,
             DeriveGeneric,StandaloneDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields -rtsopts #-}

module LLVM.Slicing.Static.Symbolic.SymADT_ where 

import GHC.Generics ( Generic )
import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf,genericRnfV1 )
import Control.Lens ( Lens', makeLenses, (.~), (%~), (^.) )
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
import LLVM.Slicing.Data.ValueDepType


import qualified LLVM.Slicing.Data.Relation as R
import qualified Data.Graph.Inductive as G

----------------------------------------------
---
type Analysis' a b = RWS (SliceEnv' a b) String SliceState

runAnalysis :: RWS r b s a -> r -> s -> a 
runAnalysis a r s = fst $ RW.evalRWS a r s
analysisEnvironment = RW.asks
analysisGet = RW.get
analysisPut = RW.put
analysisLocal = RW.local


----
data SliceEnv' a b = SEnv {   --  procSliTbl :: !(IntMap SliceTable),  
                       procValueDep :: IntMap (a b)
                     , paraValMap :: !(IntMap (Value,Int))
                     , instCtrMap :: IntMap ValueIds    -- ValIdSet
                     }                      

data SliceState = SState { traces :: !Int         --  [Instruction]
                         } deriving (Generic,NFData)

--instance NFData SliceState where
--  rnf = genericRnf 

data SliceInfo' a b = SInfo {
                       _valueDepMap :: a b 
                       } deriving (Show,Ord,Eq,Generic,NFData)

$(makeLenses ''SliceInfo')

instance IsValueDepTable a b => Monoid (SliceInfo' a b) where
  mempty = SInfo emptyValueDep 
  mappend (SInfo !vdm1) (SInfo !vdm2) = SInfo $! mrgValueDep vdm1 vdm2

top :: IsValueDepTable a b => SliceInfo' a b
top = mempty

meetSliceInfo :: IsValueDepTable a b => SliceInfo' a b -> SliceInfo' a b -> SliceInfo' a b
meetSliceInfo = {-# SCC meetSliceInfo #-}
     mappend       -- SInfo $! mrgValueDep vd1 vd2   
{-# INLINE meetSliceInfo #-}
  
--instance NFData (a b) => NFData (SliceInfo' a b) where
--  rnf = genericRnfV1  
--deriving instance (NFData (a b)) => NFData (SliceInfo' a b)
--deriving instance (Generic (a b), Generic b) => Generic (SliceInfo' a b)
--deriving instance (Show b, Show (a b),IsValueDepTable a b) => Show (SliceInfo' a b)  
--deriving instance (Eq b, Eq (a b), IsValueDepTable a b) => Eq (SliceInfo' a b) 
--deriving instance (Ord b, Ord (a b), IsValueDepTable a b) => Ord (SliceInfo' a b) 

------
data SliceSummary' a b = SliceSummary {  
                      _procValueDepSumm :: IntMap (a b)
                    , _valueDepSummary :: !(a b)
                    , _traceSize :: Int 
                    } deriving (Show,Generic,NFData)

$(makeLenses ''SliceSummary')

instance Eq (a b) => Eq (SliceSummary' a b) where
  (SliceSummary pvd1 vdm1 _) == (SliceSummary pvd2 vdm2 _) =
      pvd1 == pvd2 && vdm1 == vdm2  

instance IsValueDepTable a b => Monoid (SliceSummary' a b) where
  mempty = SliceSummary mempty emptyValueDep 0 
  mappend (SliceSummary pvd1 vdm1 tr1) (SliceSummary pvd2 vdm2 tr2) =
    SliceSummary pvd vdm (tr1 + tr2) 
    where  !pvd = IM.unionWith mrgValueDep pvd1 pvd2
           !vdm = mrgValueDep vdm1 vdm2

--instance NFData SliceSummary where
--  rnf = genericRnf

--
data SliceAnalysis' a b = SliceAnalysis { _sliceSumm :: SliceSummary' a b }
  deriving (Eq, Generic,NFData)

$(makeLenses ''SliceAnalysis')


instance IsValueDepTable a b => Monoid (SliceAnalysis' a b) where
  mempty = SliceAnalysis { _sliceSumm = mempty }
  mappend a1 a2 =
    SliceAnalysis { _sliceSumm = _sliceSumm a1 `mappend` _sliceSumm a2 }
    
 
   

     
----------------------------------
------
unionLs :: (IsValue v, IsValueDepTable a b) => 
      IntMap ValueIds -> SliceInfo' a b -> Instruction -> v -> b
unionLs cdM (SInfo vdM) i v = {-# SCC unionLs #-}  insertValIdSet iID iDeps
  where iDeps = unionLkpValueDep dds vdM   -- (si ^. valueDepMap) 
        iID = instructionUniqueId i  
        !dds = IS.toList $ IS.union vDDs iCDs
        iCDs = IM.findWithDefault IS.empty iID cdM
        vDDs = IS.insert (valueUniqueId v) (instRefs v)
{-# INLINE unionLs #-}

--unionL' :: IsValue a => ValIdSet -> ValIdSet -> (a,SliceTable) -> ValIdSet
--unionL' l0 l (v,s) = {-# SCC unionL' #-}  BDD.mrgBDDs [l0, l, refIds, unionLkpSli refStrs s]
--  where !refValues = refVals v
--        !refIds = IS.fromList . HS.toList . HS.map valueUniqueId $ refValues 
--        !refStrs = mapMaybe toVarName . HS.toList $ refValues    
--{-# INLINE unionL' #-}


----
addSliInfo :: (IsValue v, IsValueDepTable a b) => 
              v -> b -> SliceInfo' a b -> (Analysis' a b) (SliceInfo' a b)
addSliInfo v l' si =  return $! updSInfo (valueUniqueId v) l' si
--  SInfo $! updValueDep (valueUniqueId v) l' vdM

setTrSliInfo :: IsValueDepTable a b => IntMap ValueIds -> Instruction 
              -> SliceInfo' a b -> (Analysis' a b) (SliceInfo' a b)  
setTrSliInfo cdM i si = addTrSliInfo i l' si
   where   !l' = unionLs cdM si i i

addTrSliInfo :: (IsValue v, IsValueIdSet b, IsValueDepTable a b) => 
                v -> b -> SliceInfo' a b -> (Analysis' a b) (SliceInfo' a b)
addTrSliInfo v l' si = do
  addTrace'
  addSliInfo v l' si 

---
getTrace :: (Analysis' a b) Int -- [Instruction]
getTrace = do {s <- analysisGet; return (traces s) }  

addTrace :: Instruction -> (Analysis' a b) ()
addTrace i = addTrace'

addTrace' :: (Analysis' a b) ()
addTrace' = do 
  s <- analysisGet  
  let tr' = 1 + traces s   
  analysisPut s { traces = tr'}
  
---
setSliInfo :: IsValueDepTable a b => a b -> SliceInfo' a b -> (Analysis' a b) (SliceInfo' a b)
setSliInfo vdM si = return $! SInfo vdM

setWithSliInfo :: IsValueDepTable a b => a b -> SliceInfo' a b -> (Analysis' a b) (SliceInfo' a b)
setWithSliInfo vdM si = return $! modifySInfo (mrgValueDep vdM) si


--------------------------
----
modifySInfo :: IsValueDepTable a b => (a b -> a b) -> SliceInfo' a b -> SliceInfo' a b
modifySInfo modFn (SInfo vdM) = SInfo (modFn vdM)    
--
updSInfo,xtdSInfo :: IsValueDepTable a b => UniqueId -> b -> SliceInfo' a b -> SliceInfo' a b
updSInfo n ds = modifySInfo (updValueDep n ds)  
xtdSInfo n ds = modifySInfo (xtdValueDep n ds) 

updsSInfo,xtdsSInfo :: IsValueDepTable a b => [(UniqueId,b)] -> SliceInfo' a b -> SliceInfo' a b
updsSInfo nds = modifySInfo (updsValueDep nds)
xtdsSInfo nds = modifySInfo (xtdsValueDep nds)
    
updsSInfo2,xtdsSInfo2 :: IsValueDepTable a b => ValueIds -> b -> SliceInfo' a b -> SliceInfo' a b
updsSInfo2 ns ds si = IS.foldl' (\si' n -> updSInfo n ds si') si ns 
xtdsSInfo2 ns ds si = IS.foldl' (\si' n -> xtdSInfo n ds si') si ns 







----------------------------------------------------------------------------
---
instDep2SrcDepRel :: IntMap Value -> IntMap ValueIds -> R.Rel String String
instDep2SrcDepRel valMap idm = srcDepRel  
  where
    srcDepRel = S.unions [R.mkRelNeighbors a (S.toList bs)
                         | (a,bs) <- M.toList srcDepMap]
    srcDepMap = IM.foldlWithKey' doConvert mempty idm 
    doConvert acc i ls = 
      foldl' (\a v -> M.insertWith' S.union v (toSrcs ls) a) acc (toSrc i) 
    toSrc = maybeToList. valueSrc. findVal valMap
    toSrcs = S.fromList. mapMaybe valueSrc. findVals valMap

instDep2SrcLnDep :: IntMap Value -> IntMap ValueIds -> Map String [String]
instDep2SrcLnDep valMap idm = M.map (toSrcLnStr valMap) srcLnDepMap  
  where
    srcLnDepMap = IM.foldlWithKey' doConvert mempty idm 
    doConvert acc i ls = 
      foldl' (\a v -> M.insertWith' IS.union v ls a) acc (toSrcLn i) 
    toSrcLn i = [fn ++ ":" ++ show ln | 
        (fn,ln) <- maybeToList (valueSrcLn $ findVal valMap i), not(null ln)]

instDep2SrcLnDep' :: IntMap Value -> IntMap ValueIds -> IntMap ValueIds
instDep2SrcLnDep' valMap idm = IM.foldlWithKey' doConvert mempty idm
  where 
    doConvert acc i ls = 
      foldl' (\a v -> IM.insertWith IS.union v (toSrcLns ls) a) acc (toSrcLn i)    
    toSrcLns = IS.fromList. concatMap valueLine . findVals valMap    -- filter isValidInst
    toSrcLn = valueLine . findVal valMap 

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
                    
imis2r :: IntMap ValueIds -> R.Rel Int Int
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
    




     


    


