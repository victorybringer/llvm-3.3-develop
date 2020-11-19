{-# LANGUAGE FlexibleInstances,TypeSynonymInstances,MultiParamTypeClasses,
             FunctionalDependencies,TypeFamilies,FlexibleContexts,BangPatterns,
             DeriveGeneric,DeriveAnyClass,StandaloneDeriving,OverlappingInstances  #-}
{-# OPTIONS_GHC -XUndecidableInstances #-}

module LLVM.Slicing.Data.ValueDepType where

import Control.DeepSeq  
--import GHC.Generics ( Generic )
--import Control.DeepSeq.Generics ( genericRnf,genericRnfV1 )
--import System.IO.Unsafe ( unsafePerformIO ) 
import Control.Monad.Identity (runIdentity)
import Data.List  
import Data.IntSet (IntSet(..))
import qualified Data.IntSet as IS
--import Data.Map (Map)
--import qualified Data.Map as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

--import qualified LLVM.Slicing.Data.ROBDD.ROBDD as BDD
import LLVM.Slicing.Data.BDD  (ROBDD)
import qualified LLVM.Slicing.Data.BDD as BDD
import LLVM.Slicing.Util.Mix (sortFst, ValueId)

import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as UV 
import Data.Vector  (Vector,MVector)
import qualified Data.Vector as V

--import Data.Array.Repa  ( Array )
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R


--import Data.Array.Accelerate  (Acc,Plain)
import qualified Data.Array.Accelerate as A  
import qualified Data.Array.Accelerate.Utility.Sliced as A  (cons,consExp,head)
import qualified Data.Array.Accelerate.Utility.Lift.Run as A
import qualified Data.Array.Accelerate.Utility.Lift.Acc as A' 
--         (the,singleton,acc,expr,mapFst,mapSnd,unlift)
import Data.Array.Accelerate.Array.Sugar as A ( Elt(..), EltRepr )
--import Data.Array.Accelerate.Array.Lifted as A  (Vector')
import qualified Data.Array.Accelerate.Interpreter as Interp  (run,run1)
--import qualified Data.Array.Accelerate.LLVM.Native as CPU  
--import qualified Data.Array.Accelerate.IO.Repa as A



type ValueIdType = ValueId 
  
class IsValueIdSet a where
  type VSElm a
  emptyValIdSet  :: a
  elemValIdSet :: VSElm a -> a -> Bool 
  insertValIdSet :: VSElm a -> a -> a
  toListValIdSet :: a -> [VSElm a]
--  fromListValIdSet :: [VSElm a] -> a
--  sizeValIdSet :: Int
--  unitValIdSet   :: VSElm a -> a
--  unionValIdSet  :: a -> a -> a
--  unionsValIdSet :: [a] -> a
--  runValIdSet ::  a -> a
--  runMValIdSet :: Monad m => a -> m a
  
  fromListValIdSet :: [VSElm a] -> a 
  fromListValIdSet = foldl' (flip insertValIdSet) emptyValIdSet 
  emptyValIdSet = fromListValIdSet []
  
  runValIdSet :: a -> a 
  runValIdSet = id
  runMValIdSet :: Monad m => a -> m a
  runMValIdSet vs = return $! runValIdSet vs
  
  sizeValIdSet :: a -> Int
  sizeValIdSet = length . toListValIdSet  
  
  unitValIdSet   :: VSElm a -> a
  unitValIdSet v = -- runValIdSet $!
         flip insertValIdSet emptyValIdSet v
--  unitValIdSet a = fromListValIdSet [a] 
  
  unionValIdSet  :: a -> a -> a
  unionValIdSet vs1 vs2 = -- runValIdSet $! 
     if sizeValIdSet vs2 >= sizeValIdSet vs1 
     then foldl' (flip insertValIdSet) vs2 lvs1
     else foldl' (flip insertValIdSet) vs1 lvs2
    where  lvs1 = toListValIdSet vs1
           lvs2 = toListValIdSet vs2
  {-# INLINE unionValIdSet #-}
  unionsValIdSet :: [a] -> a
  unionsValIdSet vs =  -- runValIdSet $! 
       foldl' unionValIdSet emptyValIdSet vs
  {-# INLINE unionsValIdSet #-}
 
 
  
class (Eq (VDKey t),Ord (VDKey t),Show (VDKey t),IsValueIdSet b,Ord b)
  => IsValueDepTable t b where    
  data  VDTable t :: * -> *
  type  VDKey t   
--  data  VDElm t b 
--  toValIdSet :: VDElm t b -> b
  emptyValueDep :: VDTable t b
  lkpValueDep   :: VDKey t -> VDTable t b -> b
--  foldValueDep  :: (b -> b -> b) -> b -> VDTable t b -> b
  insertWithValueDep :: (b -> b -> b) -> VDKey t -> b -> VDTable t b -> VDTable t b
  mrgWithKeyValueDep :: (VDKey t -> b -> b -> b) -> 
                        VDTable t b -> VDTable t b -> VDTable t b
  mapWithKeyValueDep :: (VDKey t -> b -> b) -> VDTable t b -> VDTable t b
  toListValueDep   :: VDTable t b -> [(VDKey t,b)]
--  equalValueDep :: VDTable t b -> VDTable t b -> Bool
--  updValueDep,xtdValueDep   :: VDKey t -> b -> VDTable t b -> VDTable t b
--  mrgValueDep  :: VDTable t b -> VDTable t b -> VDTable t b 
--  mrgWithValueDep :: (b -> b -> b) -> VDTable t b -> VDTable t b -> VDTable t b
--  mrgWithKeyValueDep2 :: (IsValueDepTable t b,VDKey t ~ VSElm b,Num (VSElm b))  
--                      => [VDKey t] -> VDTable t b -> VDTable t b -> VDTable t b
--  unionLkpValueDep,unionLkpValueDep' ::  [VDKey t] -> VDTable t b -> b
--  updsValueDep,xtdsValueDep :: [(VDKey t,b)] -> VDTable t b -> VDTable t b 
--  updsValueDep2,xtdsValueDep2 :: [VDKey t] -> b -> VDTable t b -> VDTable t b
--  fromListValueDep :: [(VDKey t,b)] -> VDTable t b
--  fromIMapISetValueDep :: (VSElm b~Int,VDKey t~Int)=> IntMap IntSet -> VDTable t b
--  toIMapISetValueDep   :: (VSElm b~Int,VDKey t~Int)=> VDTable t b -> IntMap IntSet
  
  equalValueDep :: VDTable t b -> VDTable t b -> Bool
  equalValueDep t1 t2 = cmpValueDep t1 t2 == EQ
  cmpValueDep :: VDTable t b -> VDTable t b -> Ordering
  cmpValueDep t1 t2 = 
     compare (sortFst $ toListValueDep t1)(sortFst $ toListValueDep t2)
    
  mrgValueDep  :: VDTable t b -> VDTable t b -> VDTable t b 
  mrgValueDep = mrgWithValueDep unionValIdSet
  mrgWithValueDep :: (b -> b -> b) -> VDTable t b -> 
                  VDTable t b -> VDTable t b 
  mrgWithValueDep fn = mrgWithKeyValueDep (\_ b1 b2 -> fn b1 b2) 
  
  mrgWithKeyValueDep2 :: (IsValueDepTable t b,VDKey t ~ VSElm b,Num (VSElm b))  
                      => [VDKey t] -> VDTable t b -> VDTable t b -> VDTable t b
  mrgWithKeyValueDep2 glbs vdM1 vdM2 =  mrgWithKeyValueDep mapF vdM1 vdM2  
    where mapF k lx' lx = let lxs' = toListValIdSet lx' in
                          if (elem k glbs) && (notElem lxs' [[],[-k]])  
                          then lx' else unionValIdSet lx' lx
          {-# INLINE mapF #-}
          
  unionLkpValueDep',unionLkpValueDep ::  [VDKey t] -> VDTable t b -> b 
  unionLkpValueDep' ns vdM = unionsValIdSet $ map (flip lkpValueDep vdM) ns
  unionLkpValueDep ns vdM = case ns of 
           [] -> emptyValIdSet
           [n] -> lkpValueDep n vdM
           [n1,n2] -> unionValIdSet (lkpValueDep n1 vdM) (lkpValueDep n2 vdM)
           _   -> unionLkpValueDep' ns vdM 

  updValueDep,xtdValueDep  :: VDKey t -> b -> VDTable t b -> VDTable t b  
  updsValueDep,xtdsValueDep :: [(VDKey t,b)] -> VDTable t b -> VDTable t b 
  updsValueDep2,xtdsValueDep2 :: [VDKey t] -> b -> VDTable t b -> VDTable t b 
  updValueDep = insertWithValueDep const    
  xtdValueDep = insertWithValueDep unionValIdSet     
  updsValueDep nvds vdM = 
        foldl' (\vdM' (n,vd) -> updValueDep n vd vdM') vdM nvds 
  updsValueDep2 ns vd vdM =  
        foldl' (\vdM' n -> updValueDep n vd vdM') vdM ns
  xtdsValueDep nvds vdM = 
        foldl' (\vdM' (n,vd) -> xtdValueDep n vd vdM') vdM nvds 
  xtdsValueDep2 ns vd vdM =  
        foldl' (\vdM' n -> xtdValueDep n vd vdM') vdM ns 

  fromListValueDep :: [(VDKey t,b)] -> VDTable t b   
  fromListValueDep nvds = updsValueDep nvds emptyValueDep   
      
  fromIMapISetValueDep :: (VSElm b ~ Int,VDKey t ~ Int) => IntMap IntSet -> VDTable t b
  fromIMapISetValueDep idepMap = fromListValueDep $!
         [(n,fromListValIdSet $ IS.toList ls)|(n,ls) <- IM.toList idepMap] 
  toIMapISetValueDep   :: (VSElm b ~ Int,VDKey t ~ Int) => VDTable t b -> IntMap IntSet
  toIMapISetValueDep vdM = IM.fromList $!
         [(n,IS.fromList $ toListValIdSet vd)|(n,vd) <- toListValueDep vdM]
        



----
instance IsValueIdSet IntSet where
  type VSElm IntSet = Int
  emptyValIdSet  = IS.empty
  unionValIdSet  = IS.union
  {-# INLINE unionValIdSet #-}
  unionsValIdSet = IS.unions
  elemValIdSet = IS.member 
  insertValIdSet = IS.insert
  unitValIdSet   = IS.singleton
  sizeValIdSet   = IS.size
  toListValIdSet = IS.toList
  fromListValIdSet = IS.fromList

instance IsValueIdSet ROBDD where
  type VSElm ROBDD = ValueIdType
  emptyValIdSet  = BDD.makeFalse
  unionValIdSet  = BDD.mrgBDD    -- BDD.or
  unionsValIdSet = BDD.mrgBDDs
  elemValIdSet = BDD.elemBDD
  insertValIdSet = BDD.mrgBDD
  unitValIdSet   = BDD.toBDD
  toListValIdSet = BDD.fromBDD
  fromListValIdSet = BDD.toBDD

instance (Eq b,Ord b,IsValueIdSet b) => IsValueIdSet [b] where
  type VSElm [b] = b
  emptyValIdSet  = []
  elemValIdSet = elem 
  insertValIdSet v vs = if elem v vs then vs else v:vs
  unionValIdSet  = union
  unitValIdSet   = (:[])
  sizeValIdSet  = length
  toListValIdSet = id
  fromListValIdSet = nub

instance (Eq b,Ord b,IsValueIdSet b) => IsValueIdSet [(ValueIdType,b)] where
  type VSElm [(ValueIdType,b)] = (ValueIdType,b)
  emptyValIdSet  = []
  elemValIdSet = elem 
  toListValIdSet = id
  insertValIdSet = insertWith unionValIdSet
    where insertWith _ nv [] = [nv]
          insertWith fn nv@(n,v) nvs@((n',v'):nvs')
        --     | (n',v') == (n,v) = nvs 
             | n' == n    = (n, fn v v'):nvs'
             | otherwise  = (n',v'): (insertWith fn nv nvs')
  {-# INLINE insertValIdSet #-}
  unitValIdSet   = (:[])
  sizeValIdSet  = length
--  unionValIdSet  = union
  fromListValIdSet = nub  

instance (Eq b,Ord b,IsValueIdSet b) => IsValueIdSet (ValueIdType,b) where
  type VSElm (ValueIdType,b) = VSElm b
  emptyValIdSet = (0, emptyValIdSet :: IsValueIdSet b => b)     -- undefined
  elemValIdSet v (n,vs) = elemValIdSet v vs 
  insertValIdSet v (n,vs) = (n, insertValIdSet v vs)
  sizeValIdSet (n,vs) = sizeValIdSet vs
  toListValIdSet (_,vs) = toListValIdSet vs
  unionValIdSet (n1,vs1) (n2,vs2) = (n2, unionValIdSet vs1 vs2)  
  
  unionsValIdSet [] = emptyValIdSet
  unionsValIdSet nvs = (fst (head nvs), unionsValIdSet $ map snd nvs)

instance (Eq b) => IsValueIdSet (Vector b) where
  type VSElm (Vector b) = b
  emptyValIdSet  = V.empty
  elemValIdSet = V.elem 
  insertValIdSet v vs = 
     if V.elem v vs then vs else V.cons v vs
--  unionValIdSet  = (V.++)
--  unionsValIdSet = V.concat
  unitValIdSet   = V.singleton
  sizeValIdSet   = V.length
  toListValIdSet = V.toList
  fromListValIdSet = V.fromList . nub


------- Accelerate
---- 
runAcc,run :: A.Arrays a => A.Acc a -> a
runAcc =  Interp.run . A.compute     --CPU.run 
run = runAcc

run1,run1' :: (A.Arrays a, A.Arrays b) => (A.Acc a -> A.Acc b) -> a -> b
run1 f = Interp.run1 (f A.>-> A.compute)  --   -- CPU.run1 
run1' f = Interp.run1 (A.compute A.>-> f A.>-> A.compute) 

run2 :: (A.Arrays a, A.Arrays b, A.Arrays c) => 
     (A.Acc a -> A.Acc b -> A.Acc c) -> a -> b -> c
run2 f =  let !go = run1 (A.uncurry f)
          in \x y -> go (x, y)
--runs :: (A.Arrays a, A.Arrays b) => (A.Acc a -> A.Acc b) -> [a] -> [b]
--runs f = CPU.stream ( f A.>-> A.compute)

fromExp :: A.Elt a => a -> A.Exp a -> a
fromExp a0 x = if null xs then a0 else head xs
   where  xs = A.toList. runAcc $ A.unit x
   
fromExp' :: A.Elt a => A.Exp a -> a
fromExp' = A'.the . runAcc . A.unit
fromScalar :: (A.Elt a) => A.Scalar a -> a
fromScalar = flip A.indexArray A.Z   -- A'.the


--eqAcc :: (A.IsScalar e,Elt e) => AccVector e -> AccVector e -> A.Acc (A.Scalar Bool)
eqAcc arr1 arr2 = A.unit $ (A.size arr1 A.== A.size arr2)
                  A.&& (A.the $ A.and (A.zipWith (A.==) arr1 arr2)) 


------------
type AVector a = A.Vector a 

instance (Elt b,A.IsScalar b)  => Eq (AVector b) where
    bs1 == bs2  = A'.the $! run2 eqAcc bs1 bs2

instance (Elt b,Ord b,A.IsScalar b)  => Ord (AVector b) where
    compare vs1 vs2  = compare (toListValIdSet vs1) (toListValIdSet vs2)

instance (Elt b,A.IsScalar b,Eq b) => IsValueIdSet (AVector b) where
  type VSElm (AVector b) = b  
  emptyValIdSet = fromListValIdSet []  
  runValIdSet vs = Interp.run1 A.compute vs
  elemValIdSet v vs = A'.the $! run1 isElm vs
     where isElm = A.any (A.== (A.constant v))
     
  insertValIdSet v vs = if elemValIdSet v vs then vs
                        else (A.with run1 A.consExp) v vs
  
--  unionValIdSet vs1 vs2 = 
--     if sizeValIdSet vs2' >= sizeValIdSet vs1' 
--     then foldl' insert vs2' lvs1
--     else foldl' insert vs1' lvs2
--    where  lvs1 = A.toList vs1'
--           lvs2 = A.toList vs2'
--           vs1' = runValIdSet vs1
--           vs2' = runValIdSet vs2
--           insert vs v = 
--               if elemValIdSet v vs then vs
--               else (A.with run1 A.consExp) v vs
--  {-# INLINE unionValIdSet #-}
     
  unitValIdSet v  = fromListValIdSet [v]
  sizeValIdSet    = A.arraySize. A.arrayShape
  toListValIdSet  = A.toList . runValIdSet 
  fromListValIdSet vls =  let vls' = nub vls in
        A.fromList (A.Z A.:. (length vls')) vls' 


-----
type AccVector a = A.Acc (A.Vector a)
          
--instance (IsValueIdSet (AccVector b),Ord b) => Eq (AccVector b) where
--    bs1 == bs2  =  sort (toListValIdSet bs1) == sort (toListValIdSet bs2) 

instance (Elt b,A.IsScalar b) => Eq (AccVector b) where
    vs1 == vs2  =  A'.the . runAcc $! eqAcc vs1 vs2

instance (Ord b, A.IsScalar b,Elt b,IsValueIdSet (AccVector b))
   => Ord (AccVector b) where
    compare vs1 vs2  = compare (toListValIdSet vs1) (toListValIdSet vs2)
    
instance (Elt b,A.IsScalar b,Eq b) => IsValueIdSet (AccVector b) where
  type VSElm (AccVector b) = b  
  emptyValIdSet = fromListValIdSet []  
  runValIdSet = A.compute
  elemValIdSet v vs = A'.the $! runAcc isElm
     where isElm = A.any (A.== (A.constant v)) vs 
     
  insertValIdSet v vs = 
     if elemValIdSet v vs then vs 
     else A.consExp (A.constant v) vs  
     
  unitValIdSet v  = fromListValIdSet [v]
  sizeValIdSet    = fromExp 0 . A.length
  toListValIdSet  = A.toList . runAcc 
  fromListValIdSet vls = let vls' = nub vls in
      A.use $ A.fromList (A.Z A.:. (length vls')) vls' 




---------- Repa
----
type RVector a = R.Array R.U R.DIM1 a

--instance (Eq b,Unbox b) => Eq (RVector b) where
--    (==)  =  R.equalsS

instance (Ord b, A.IsScalar b,Unbox b,IsValueIdSet (RVector b))
   => Ord (RVector b) where
    compare bs1 bs2  =  compare (toListValIdSet bs1) (toListValIdSet bs2)

instance (Eq b,Unbox b) => IsValueIdSet (RVector b) where
  type VSElm (RVector b) = b
  emptyValIdSet  = fromListValIdSet []
  runValIdSet  = runIdentity . R.computeP . R.delay
  elemValIdSet v vs = runIdentity. R.foldAllP (||) False $ R.map (== v) vs  
  insertValIdSet v vs = 
       if elemValIdSet v vs then vs else vs'
     where  uvs = UV.cons v . R.toUnboxed $ vs
            idx = R.Z R.:. (sizeValIdSet vs + 1) 
            vs' = R.fromUnboxed idx uvs 
  unitValIdSet v  = fromListValIdSet [v]
  sizeValIdSet   = R.size . R.extent
  toListValIdSet = R.toList . R.delay   -- UV.toList . R.toUnboxed
  fromListValIdSet vls = let vls' = nub vls in    -- R.delay $ 
       R.fromListUnboxed (R.Z R.:. (length vls')) vls'
 
    


------------------------------------------------------
------------ Instances For IsValueDepTable -----------
---
data IntMap_  = IntMap_ deriving (Show,Read)
data Vector_  = Vector_ deriving (Show,Read)
data AccArr_  = AccArr_ deriving (Show,Read) 
data RepaArr_ = RepaArr_ deriving (Show,Read)
data MVector_ = MVector_ deriving (Show,Read) 

   
--instance (Generic b,IsValueDepTable a b) => Generic (VDTable a b)
instance (Show b, IsValueDepTable a b) => Show (VDTable a b) where 
   show vm = "VDTable: fromList " ++ show (toListValueDep vm)
instance (Ord b,IsValueDepTable a b) => Eq (VDTable a b) where
   (==)  = equalValueDep 
instance (Ord b,IsValueDepTable a b) => Ord (VDTable a b) where
   compare = cmpValueDep 
instance IsValueDepTable a b => NFData (VDTable a b) where
   rnf !t = t `seq` ()     -- genericRnf  




instance (Ord b,IsValueIdSet b) => IsValueDepTable IntMap_ b where
  data VDTable IntMap_ v = VD_IntMap (IntMap v)  -- deriving (Show,Ord,Eq,NFData)
  type VDKey IntMap_ = Int 
  emptyValueDep = VD_IntMap IM.empty 
  equalValueDep (VD_IntMap vdM1)(VD_IntMap vdM2) = vdM1 == vdM2
  cmpValueDep (VD_IntMap vdM1)(VD_IntMap vdM2) = compare vdM1 vdM2  
  lkpValueDep vn (VD_IntMap vdM) = IM.findWithDefault emptyValIdSet vn vdM
--  foldValueDep fn v (VD_IntMap vdM) = IM.foldr' fn v vdM 
  mapWithKeyValueDep fn (VD_IntMap vdM) = VD_IntMap (IM.mapWithKey fn vdM)
  toListValueDep (VD_IntMap vdM) = IM.toList vdM   
  mrgWithKeyValueDep fn (VD_IntMap vdM1) (VD_IntMap vdM2) = 
         VD_IntMap (IM.unionWithKey fn vdM1 vdM2)
  insertWithValueDep fn n v (VD_IntMap vdM) = VD_IntMap (IM.insertWith fn n v vdM)
--  {-# INLINE xtdValueDep #-}          
  unionLkpValueDep' vs (VD_IntMap vdM)  = let keyF k _ = elem k vs  in
                unionsValIdSet. IM.elems $ IM.filterWithKey keyF vdM
  {-# INLINE unionLkpValueDep' #-}


instance (Ord b,IsValueIdSet b) => IsValueDepTable Vector_ b where
  data VDTable Vector_ b  = VD_Vector (Vector (ValueIdType,b))
  type VDKey Vector_ = ValueIdType
  emptyValueDep = VD_Vector V.empty    
  equalValueDep (VD_Vector nvs1)(VD_Vector nvs2) = nvs1 == nvs2
  cmpValueDep (VD_Vector nvs1)(VD_Vector nvs2) = compare nvs1 nvs2
--  foldValueDep fn v (VD_Vector nvs) =  V.foldr' mapF v nvs 
--     where  mapF (n,v1) v2 = fn v1 v2
  lkpValueDep n (VD_Vector nvs) =    
     unionsValIdSet. map snd . V.toList $ V.filter ((==n). fst) nvs
--     where  (ns,vs) = V.unzip nvs 
--            idxs = V.elemIndices n ns
--            n_vs = V.force $ V.backpermute vs idxs
--            result = unionsValIdSet . map snd $ V.toList n_vs
  insertWithValueDep fn n v (VD_Vector nvs) = 
     if V.null nvs then VD_Vector (V.singleton (n,v)) else VD_Vector nvs'
     where  (nvs1,nvs2) = V.partition ((==n). fst) nvs
            n_v = unionsValIdSet . map snd $ V.toList nvs1
            nvs' = if V.null nvs1 
                   then V.cons (n,v) nvs
                   else V.snoc nvs2 (n, fn v n_v)             
  {-# INLINE insertWithValueDep #-}  
  
  mrgWithKeyValueDep fn vs1@(VD_Vector nvs1) vs2@(VD_Vector nvs2) = vs'
     where vs' = if V.length nvs2 >= V.length nvs1 
                 then V.foldr' mapF vs2 nvs1 
                 else V.foldr' mapF vs1 nvs2
           mapF (n,v) vs = insertWithValueDep (fn n) n v vs 
                 
  mapWithKeyValueDep fn (VD_Vector nvs) = VD_Vector nvs'
     where nvs' = V.map mapF nvs
           mapF (n,v) = (n,fn n v)           
  toListValueDep (VD_Vector nvs) = V.toList nvs
  fromListValueDep = VD_Vector . V.fromList


-------- Accelerate

-- IntSet to Accelerate
type instance EltRepr IntSet  = Int
instance Elt IntSet where
  eltType _  = eltType (undefined :: Int)
  toElt     = IS.singleton 
  fromElt x  = if null ns then 0 else head ns 
     where  ns = IS.toList x

---
instance (Ord b,IsValueIdSet b,A.Elt b) 
 => IsValueDepTable AccArr_ b where
  data VDTable AccArr_ b  = VD_AccArr (AccVector (ValueIdType,b))
  type VDKey AccArr_ = ValueIdType
  emptyValueDep = fromListValueDep [] 
--  foldValueDep fn v (VD_AccArr avs) = head . A.toList. runAcc $ vs
--      where  vs = A.fold (A.lift2 fn) (A.constant v)(snd $ A.unzip avs) 
  lkpValueDep n (VD_AccArr nvs) =  vs
     where  vs = unionsValIdSet. A.toList $ runAcc anv           
            anv = snd . A.unzip $ A.filter mapF nvs 
            mapF v =  (A.fst v) A.== (A.constant n)
  insertWithValueDep fn n v (VD_AccArr nvs) = VD_AccArr nvs'
     where  nvs' = A.consExp nve nvs2      
            nvs1 = A.filter filterF nvs
            nvs2 = A.filter (A.not . filterF) nvs
            filterF nv = (A.fst nv) A.== (A.constant n)             
            nve = A.constant (n, fn v n_v)   
            n_v = unionsValIdSet. A.toList. runAcc. snd $ A.unzip nvs1 
  {-# INLINE insertWithValueDep #-}  
  
  mrgWithKeyValueDep fn vs1@(VD_AccArr nvs1) vs2@(VD_AccArr nvs2) = vs'  
     where vs' = if (fromExp False $ (A.size nvs2) A.>= (A.size nvs1))  
                 then (foldl' insertFn vs2 $ toListValueDep vs1)
                 else (foldl' insertFn vs1 $ toListValueDep vs2)
           insertFn vs (n,v) = insertWithValueDep (fn n) n v vs 
   
  mapWithKeyValueDep fn (VD_AccArr nvs) = VD_AccArr nvs'
     where (ns,vs) = A.unzip nvs
           vs' = A.zipWith mapF ns vs
           mapF n v = A.constant $ fn (fromExp 0 n) (fromExp emptyValIdSet v)
           nvs' = A.zip ns vs'
           
  toListValueDep (VD_AccArr nvs) = A.toList (runAcc nvs)
  fromListValueDep vls = VD_AccArr . A.use $ A.fromList (A.Z A.:. (length vls)) vls
 



---------------
split :: (A.IsNum e, A.Elt e) =>  (A.Exp e -> A.Exp Bool)
           -> AccVector e -> (AccVector e, AccVector e)
split cond arr = (yesArr,noArr)
  where (yesArr,noArr) = (A.drop (A.the len) arr', A.take (A.the len) arr')
        arr' = A.scatter idx initArr arr
        idx = A.zipWith (\f b -> f A.== 0 A.? (A.fst b, A.snd b)) flags (A.zip fIdx' fIdx)
        flags = A.map (A.boolToInt. cond) arr
        flags' = A.map (1-) flags
        (fIdx',len) = A.scanl' (+) 0 flags'
        fIdx = A.tail $ A.map (\x -> (A.size arr)-1-x) $ A.scanr (+) 0 flags
        initArr = A.generate (A.shape arr) (const 0)


        

------- Test
vs0 :: VDTable IntMap_ (AVector Int)
vs0 = fromListValueDep [(2,fromListValIdSet [1::Int]), (9,fromListValIdSet [2,3,1]),
                  (6,fromListValIdSet [1,3,2,3,4,4,4,4,4,4]),(5,fromListValIdSet [4])]

v6 = lkpValueDep 6 vs0
v6' = insertValIdSet 6 v6

vs1 :: VDTable Vector_ IntSet
vs1 = fromListValueDep [(2,IS.fromList [1]), (9,IS.fromList [2,3,1]),
                  (6,IS.fromList [1,3,2,3,4,4,4,4,4,4]),(5,IS.fromList [4])]

vs2 = updsValueDep2 [3,6] (IS.fromList [3,4,6,9]) vs1
vs3 = xtdValueDep 6 (IS.fromList [1..4]) vs1

arr = A.fromList (A.Z A.:. 8) [5,7,3,1,4,2,7,2] :: A.Vector Int
(arr1,arr2) = split A.even (A.use arr)



