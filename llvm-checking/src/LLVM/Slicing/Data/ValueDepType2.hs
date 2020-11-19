{-# LANGUAGE FlexibleInstances,TypeSynonymInstances,MultiParamTypeClasses,
             FunctionalDependencies,TypeFamilies,FlexibleContexts,BangPatterns,
             DeriveGeneric,DeriveAnyClass,StandaloneDeriving   #-}
{-# OPTIONS_GHC -XUndecidableInstances #-}

module LLVM.Slicing.Data.ValueDepType2 where
 
import GHC.Generics ( Generic )
import Control.DeepSeq 
--import Control.DeepSeq.Generics ( genericRnf,genericRnfV1 )
 
import Data.List  
import Data.IntSet (IntSet(..))
import qualified Data.IntSet as IS
--import Data.Map (Map)
--import qualified Data.Map as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

--import qualified LLVM.Slicing.Data.ROBDD.ROBDD as BDD
import LLVM.Slicing.Data.BDD  ( ROBDD )
import qualified LLVM.Slicing.Data.BDD as BDD

import Data.Array.Repa  ( Array )
import qualified Data.Array.Repa as Repa
import Data.Vector  (Vector,MVector)
import qualified Data.Vector as Vec

  
class Eq a => IsValueIdSet a where
  type VSElm a
  emptyValIdSet  :: a
  unionValIdSet  :: a -> a -> a
--  unionsValIdSet :: [a] -> a
  memberValIdSet :: VSElm a -> a -> Bool
  insertValIdSet :: VSElm a -> a -> a
  unitValIdSet   :: VSElm a -> a
  toListValIdSet :: a -> [VSElm a]
  fromListValIdSet :: [VSElm a] -> a
  
  unionsValIdSet :: [a] -> a
  unionsValIdSet = foldl' unionValIdSet emptyValIdSet
  
  
class (Eq (VDKey t),Ord (VDKey t),Show (VDKey t),IsValueIdSet b) => IsValueDepTable t b where
  data  VDTable t :: * -> *
  type  VDKey t
  emptyValueDep :: VDTable t b
  lkpValueDep   :: VDKey t -> VDTable t b -> b
  insertWithValueDep :: (b -> b -> b) -> VDKey t -> b -> VDTable t b -> VDTable t b
  mrgWithKeyValueDep :: (VDKey t -> b -> b -> b) -> VDTable t b -> VDTable t b -> VDTable t b
  mapWithKeyValueDep :: (VDKey t -> b -> c) -> VDTable t b -> VDTable t c
  toListValueDep   :: VDTable t b -> [(VDKey t,b)]
--  updValueDep,xtdValueDep   :: VDKey t -> b -> VDTable t b -> VDTable t b
--  mrgValueDep  :: VDTable t b -> VDTable t b -> VDTable t b 
--  mrgWithValueDep :: (b -> b -> b) -> VDTable t b -> VDTable t b -> VDTable t b
--  unionLkpValueDep,unionLkpValueDep' ::  [VDKey t] -> VDTable t b -> b
--  updsValueDep,xtdsValueDep :: [(VDKey t,b)] -> VDTable t b -> VDTable t b 
--  updsValueDep2,xtdsValueDep2 :: [VDKey t] -> b -> VDTable t b -> VDTable t b
--  fromListValueDep :: [(VDKey t,b)] -> VDTable t b
--  fromIMapISetValueDep :: IntMap IntSet -> VDTable t b
--  toIMapISetValueDep   :: VDTable t b -> IntMap IntSet

  mrgValueDep  :: VDTable t b -> VDTable t b -> VDTable t b 
  mrgValueDep = mrgWithValueDep unionValIdSet
  mrgWithValueDep :: (b -> b -> b) -> VDTable t b -> VDTable t b -> VDTable t b 
  mrgWithValueDep fn = mrgWithKeyValueDep (\_ b1 b2 -> fn b1 b2)
  mrgWithKeyValueDep2 :: (VSElm b ~ Int,VDKey t ~ Int) => [Int] -> 
                 VDTable t b -> VDTable t b -> VDTable t b
  mrgWithKeyValueDep2 glbs vdM1 vdM2 =  mrgWithKeyValueDep mapF vdM1 vdM2  
    where mapF k lx' lx = if (elem k glbs) && (notElem lx' [emptyValIdSet, unitValIdSet (-k)])  
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
        
 
--instance (Generic b,IsValueDepTable a b) => Generic (VDTable a b)
instance (Show b, IsValueDepTable a b) => Show (VDTable a b) where 
   show vm = "VDTable: fromList " ++ show (toListValueDep vm)
instance (Eq b,IsValueDepTable a b) => Eq (VDTable a b) where
   vm1 == vm2  = toListValueDep vm1 == toListValueDep vm2
instance (Ord b,IsValueDepTable a b) => Ord (VDTable a b) where
   compare vm1 vm2  = compare (toListValueDep vm1) (toListValueDep vm2)      
instance IsValueDepTable a b => NFData (VDTable a b) where
   rnf !_ = ()     -- vm `seq` ()  



----
instance IsValueIdSet IntSet where
  type VSElm IntSet = Int
  emptyValIdSet  = IS.empty
  unionValIdSet  = IS.union
  {-# INLINE unionValIdSet #-}
  unionsValIdSet = IS.unions
  memberValIdSet = IS.member 
  insertValIdSet = IS.insert
  unitValIdSet   = IS.singleton
  toListValIdSet = IS.toList
  fromListValIdSet = IS.fromList

instance IsValueIdSet ROBDD where
  type VSElm ROBDD = Int
  emptyValIdSet  = BDD.makeFalse
  unionValIdSet  = BDD.mrgBDD    -- BDD.or
  unionsValIdSet = BDD.mrgBDDs
  memberValIdSet = BDD.elemBDD
  insertValIdSet = BDD.mrgBDD
  unitValIdSet   = BDD.toBDD
  toListValIdSet = BDD.fromBDD
  fromListValIdSet = BDD.toBDD

instance IsValueIdSet [Int] where
  type VSElm [Int] = Int
  emptyValIdSet  = []
  unionValIdSet  = union
  memberValIdSet = elem 
  insertValIdSet = insert
  unitValIdSet   = (:[])
  toListValIdSet = id
  fromListValIdSet = id

instance (Eq b,Ord b,IsValueIdSet b) => IsValueIdSet [(Int,b)] where  
  type VSElm [(Int,b)] = (Int,b)
  emptyValIdSet  = []
  unionValIdSet  = union
  memberValIdSet = elem 
  insertValIdSet = insert
  unitValIdSet   = (:[])
  toListValIdSet = id
  fromListValIdSet = id

  
---
data IntMap_ = IntMap_ deriving (Show,Read)
data Vector_ = Vector_ deriving (Show,Read)
data MVector_ = MVector_ deriving (Show,Read)
data Vector_BDD = Vector_BDD deriving (Show,Read)

instance IsValueIdSet b => IsValueDepTable IntMap_ b where
  data VDTable IntMap_ v = VD_IntMap (IntMap v) 
  type VDKey IntMap_ = Int
  emptyValueDep = VD_IntMap IM.empty 
  lkpValueDep vn (VD_IntMap vdM) = IM.findWithDefault emptyValIdSet vn vdM
  mapWithKeyValueDep fn (VD_IntMap vdM) = VD_IntMap (IM.mapWithKey fn vdM)
  toListValueDep (VD_IntMap vdM) = IM.toList vdM
  mrgWithKeyValueDep fn (VD_IntMap vdM1) (VD_IntMap vdM2) = 
         VD_IntMap (IM.unionWithKey fn vdM1 vdM2)
  insertWithValueDep fn n v (VD_IntMap vdM) = VD_IntMap (IM.insertWith fn n v vdM)
  {-# INLINE xtdValueDep #-}          
  unionLkpValueDep' vs (VD_IntMap vdM)  = let keyF k _ = elem k vs  in
                unionsValIdSet. IM.elems $ IM.filterWithKey keyF vdM
  {-# INLINE unionLkpValueDep' #-}

instance IsValueIdSet b => IsValueDepTable Vector_ b where
  data VDTable Vector_ v  = VD_Vector (Vector v)  
  type VDKey Vector_ = Int
  emptyValueDep = VD_Vector Vec.empty 
  lkpValueDep  = undefined 
  insertWithValueDep = undefined
  mrgWithKeyValueDep = undefined
  mapWithKeyValueDep = undefined
  toListValueDep = undefined

instance IsValueDepTable Vector_BDD ROBDD where
  data VDTable Vector_BDD v  = VD_Vector_BDD (Vector v)    
  type VDKey Vector_BDD = Int 
  emptyValueDep = VD_Vector_BDD Vec.empty 
  lkpValueDep  = undefined 
  insertWithValueDep = undefined
  mrgWithKeyValueDep = undefined
  mapWithKeyValueDep = undefined
  toListValueDep = undefined



  