{-# LANGUAGE FlexibleInstances,TypeSynonymInstances,MultiParamTypeClasses,FunctionalDependencies  #-}
{-# OPTIONS_GHC -XUndecidableInstances #-}

module LLVM.Slicing.Data.ValueDepType ( 
  -- * Types
  IsValueIdSet(..), 
  IsValueDepTable(..)
  )
 where 
 
import Data.List  
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
--import Data.Map (Map)
--import qualified Data.Map as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

--import qualified LLVM.Slicing.Data.ROBDD.ROBDD as BDD
--import LLVM.Slicing.Data.ROBDD.Types  ( ROBDD )
import qualified LLVM.Slicing.Data.BDD as BDD

import qualified Data.Array.Repa as Repa
import qualified Data.Vector as Vec



type ValueId = Int
--type ValueIdSet = IntSet  
--type ValueDepTable = IntMap ValueIdSet 
  
class IsValueIdSet a where
  emptyValIdSet  :: a
  unionValIdSet  :: a -> a -> a
--  unionsValIdSet :: [a] -> a
  memberValIdSet :: ValueId -> a -> Bool
  insertValIdSet :: ValueId -> a -> a
  unitValIdSet   :: ValueId -> a
  toListValIdSet :: a -> [ValueId]
  fromListValIdSet :: [ValueId] -> a
  
  unionsValIdSet :: [a] -> a
  unionsValIdSet = foldl' unionValIdSet emptyValIdSet
  
  
class IsValueIdSet b => IsValueDepTable a b | b -> a where
  emptyValueDep :: a b
  lkpValueDep   :: ValueId -> a b -> b
  insertWithValueDep :: (b -> b -> b) -> ValueId -> b -> a b -> a b
  mrgWithKeyValueDep :: (ValueId -> b -> b -> b) -> a b -> a b -> a b
  mapWithKeyValueDep :: IsValueIdSet c => (ValueId -> b -> c) -> a b -> a c
  toListValueDep   :: a b -> [(ValueId,b)]
--  updValueDep,xtdValueDep   :: ValueId -> b -> a b -> a b
--  mrgValueDep  :: a b -> a b -> a b 
--  mrgWithValueDep :: (b -> b -> b) -> a b -> a b -> a b
--  mrgWithKeyValueDep2 :: [ValueId] -> a b -> a b -> a b
--  unionLkpValueDep,unionLkpValueDep' ::  [ValueId] -> a b -> b
--  updsValueDep,xtdsValueDep :: [(ValueId,b)] -> a b -> a b 
--  updsValueDep2,xtdsValueDep2 :: [ValueId] -> b -> a b -> a b
--  fromListValueDep :: [(ValueId,b)] -> a b
--  fromIMapISetValueDep :: IntMap IntSet -> a b
--  toIMapISetValueDep   :: a b -> IntMap IntSet

  mrgValueDep  :: a b -> a b -> a b 
  mrgValueDep = mrgWithValueDep unionValIdSet
  mrgWithValueDep :: (b -> b -> b) -> a b -> a b -> a b 
  mrgWithValueDep fn = mrgWithKeyValueDep (\_ b1 b2 -> fn b1 b2)
  mrgWithKeyValueDep2 :: [ValueId] -> a b -> a b -> a b
  mrgWithKeyValueDep2 glbs vdM1 vdM2 = mrgWithKeyValueDep mapF vdM1 vdM2  
    where mapF k lx' lx = let lxs' = toListValIdSet lx' in
                          if (elem k glbs) && (notElem lxs' [[],[-k]])  
                          then lx' else unionValIdSet lx' lx
          {-# INLINE mapF #-}
          
  unionLkpValueDep',unionLkpValueDep ::  [ValueId] -> a b -> b 
  unionLkpValueDep' ns vdM = unionsValIdSet $ map (flip lkpValueDep vdM) ns
  unionLkpValueDep ns vdM = case ns of 
           [] -> emptyValIdSet
           [n] -> lkpValueDep n vdM
           [n1,n2] -> unionValIdSet (lkpValueDep n1 vdM) (lkpValueDep n2 vdM)
           _   -> unionLkpValueDep' ns vdM 

  updValueDep,xtdValueDep  :: ValueId -> b -> a b -> a b  
  updsValueDep,xtdsValueDep :: [(ValueId,b)] -> a b -> a b 
  updsValueDep2,xtdsValueDep2 :: [ValueId] -> b -> a b -> a b 
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

  fromListValueDep :: [(ValueId,b)] -> a b   
  fromListValueDep nvds = updsValueDep nvds emptyValueDep   
      
  fromIMapISetValueDep :: IntMap IntSet -> a b
  fromIMapISetValueDep idepMap = fromListValueDep $!
         [(n,fromListValIdSet $ IS.toList ls)|(n,ls) <- IM.toList idepMap] 
  toIMapISetValueDep   :: a b -> IntMap IntSet
  toIMapISetValueDep vdM = IM.fromList $!
         [(n,IS.fromList $ toListValIdSet vd)|(n,vd) <- toListValueDep vdM]
        

----
instance IsValueIdSet IntSet where
  emptyValIdSet  = IS.empty
  unionValIdSet  = IS.union
  {-# INLINE unionValIdSet #-}
  unionsValIdSet = IS.unions
  memberValIdSet = IS.member 
  insertValIdSet = IS.insert
  unitValIdSet   = IS.singleton
  toListValIdSet = IS.toList
  fromListValIdSet = IS.fromList

instance IsValueIdSet BDD.ROBDD where
  emptyValIdSet  = BDD.makeFalse
  unionValIdSet  = BDD.mrgBDD    -- BDD.or
  unionsValIdSet = BDD.mrgBDDs
  memberValIdSet = BDD.elemBDD
  insertValIdSet = BDD.mrgBDD
  unitValIdSet   = BDD.toBDD
  toListValIdSet = BDD.fromBDD
  fromListValIdSet = BDD.toBDD


instance IsValueIdSet [Int] where
  emptyValIdSet  = []
  unionValIdSet  = union
  memberValIdSet = elem 
  insertValIdSet = insert
  unitValIdSet   = (:[])
  toListValIdSet = id
  fromListValIdSet = id

instance (Eq b, IsValueIdSet b) => IsValueIdSet [(Int,b)] where
  emptyValIdSet  = []
  unionValIdSet  = union
--  memberValIdSet = elem 
--  insertValIdSet = insert
--  unitValIdSet   = (:[])
  toListValIdSet = toListValIdSet. unionsValIdSet. map snd
--  fromListValIdSet = id

  
---
--instance IsValueIdSet b => IsValueDepTable IntMap b where
--  emptyValueDep = IM.empty 
--  lkpValueDep vn vdM = IM.findWithDefault emptyValIdSet vn vdM
--  mapWithKeyValueDep = IM.mapWithKey
--  toListValueDep = IM.toList
--  mrgWithValueDep = IM.unionWith 
--  mrgWithKeyValueDep = IM.unionWithKey 
--  insertWithValueDep = IM.insertWith
--  {-# INLINE xtdValueDep #-}          
--  unionLkpValueDep' vs vdM  = let keyF k _ = elem k vs  in
--                unionsValIdSet. IM.elems $ IM.filterWithKey keyF vdM
--  {-# INLINE unionLkpValueDep' #-}

instance IsValueIdSet b => IsValueDepTable Vec.Vector b where
  emptyValueDep = Vec.empty 
  lkpValueDep vn vdM = undefined 
  insertWithValueDep = undefined
  mrgWithKeyValueDep = undefined
  mapWithKeyValueDep = undefined
  toListValueDep = undefined





 