{-# LANGUAGE ScopedTypeVariables,BangPatterns,NoMonomorphismRestriction,RankNTypes,
     FlexibleInstances,UndecidableInstances,IncoherentInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module LLVM.Slicing.Data.BDD (
 BDDable(..),
 mrgBDD, mrgBDDs, bddUnion, bddUnions, 
 mrgByBDD, mrgISByBDD,
 bddElem, elemBDD,
 getBDD, toROBDD,
 encodeBDD, decodeBDD,
 writeBDD, readBDD, 
 cachedBDD,
 --
 showBDD, printBDD,

 module LLVM.Slicing.Data.ROBDD.ROBDD,
 module LLVM.Slicing.Data.ROBDD.Types
 )where

import System.Environment
import System.FilePath
import System.Process (system)
import System.Directory (doesFileExist)
--import qualified Control.Monad.Trans.State.Strict as ST
import Prelude hiding(and,or,not)
--import qualified Prelude as P (and,or,not)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits  hiding (xor)
--import Data.Foldable hiding (and,or)
import qualified Data.List as List (partition)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List (sortBy,foldl')
import Data.Function (on)
import Data.Monoid 
import Data.GraphViz

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Control.Monad (liftM)
import Control.DeepSeq
--import Control.DeepSeq.Generics ( genericRnf )

import LLVM.Slicing.Data.ROBDD.ROBDD 
import LLVM.Slicing.Data.ROBDD.Types 

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS   ( toChunks )
import qualified Data.ByteString.Lazy.Internal as LS  ( ByteString(..) )
--import qualified Network.Memcache as MC
--import Control.Monad.Trans.Resource (allocate, release, runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)

--import Data.Unique
--import System.IO.Unsafe (unsafePerformIO)
--newUID = hashUnique $ unsafePerformIO newUnique

--import Debug.Trace
--
--debug :: c -> String -> c
--debug = flip trace

------------------------------------------
class BDDable a where
  toBDD :: a -> ROBDD
  fromBDD :: ROBDD -> a

instance BDDable ROBDD where
  toBDD = id
  fromBDD = id

instance BDDable BDD where
  toBDD = toROBDD
  fromBDD = getBDD 

instance BDDable Int where
  toBDD = encInt 
  fromBDD r =  let ls = decIntSet r in
      if IS.size ls == 1 
      then head (IS.elems ls) 
      else (error "Not a singleton Int, or a Zero/One!")

instance BDDable Integer where
  toBDD = encInt . toInt
  fromBDD r =  let ls = decIntSet r in
      if IS.size ls == 1 
      then toInteger . head $ IS.elems ls 
      else (error "Not a singleton Integer, or a Zero/One!")

instance BDDable IntSet where
  toBDD = encIntSet
  fromBDD = decIntSet
  
instance (Ord a, FiniteBits a,Num a) => BDDable (Set a) where
  toBDD = encSet
  fromBDD = decSet

instance (FiniteBits a,Hashable a,Ord a,Num a) => BDDable (HashSet a) where
  toBDD = encHashSet
  fromBDD = decHashSet

--instance (Ord a, FiniteBits a, Num a) => BDDable a where
--  toBDD = encSet . Set.singleton
--  fromBDD r = let (as :: Set a)= decSet r in
--     if (Set.size as) == 1 
--     then Set.elemAt 0 as 
--     else (error "Not a singleton or Zero/One!") 

instance (Ord a, FiniteBits a,Num a) => BDDable [a] where
  toBDD = encSet . Set.fromList
  fromBDD = Set.toList . decSet

instance BDDable [Int] where
  toBDD = encIntSet . IS.fromList
  fromBDD = IS.toList . decIntSet

instance {-# OVERLAPS #-} BDDable [Integer] where
  toBDD = encIntSet . IS.fromList . map toInt
  fromBDD = map toInteger . IS.toList . decIntSet

-----
instance Ord ROBDD where
 compare (ROBDD _ _ b1) (ROBDD _ _ b2) = bddCmp b1 b2

instance NFData ROBDD where
  rnf (ROBDD m ls bdd) = rnf (m,(),bdd) 

instance NFData BDD where
  rnf Zero = ()
  rnf One = ()
  rnf (BDD l v h i ih) = rnf (l,v,h,i,ih) 

-----
instance (Eq k, Hashable k, Binary k, Binary e) => Binary (HashMap k e) where
    put m = put (HM.size m) >> mapM_ put (HM.toList m)
    get   = liftM HM.fromList get  

-----------------------  
-----
encIntSet :: IntSet -> ROBDD
encIntSet ls = ROBDD (bddRevMap s) (bddIdSource s) bdd
   where (bdd, s) = runBDDContext (encIntSet' btSize ls) emptyBDDState
         btSize = finiteBitSize (undefined :: Int)
{-# INLINE encIntSet #-}

encIntSet1 ls = ROBDD (bddRevMap s) (bddIdSource s) bdd
  where (bdd, s) = runBDDContext (encIntSet' (getSize $ IS.findMax ls) ls) emptyBDDState

encIntSet' btSize = encIntSet'' btSize (btSize - 1)    

encIntSet'' :: Int -> Int -> IntSet -> BDDContext Int BDD BDD
encIntSet'' btSize pos elems 
  | IS.null elems = return Zero
  | pos == -1 = return One
  | otherwise = {- memoize var $ -} do
      t1 <- encIntSet'' btSize (pos-1) l_els
      t2 <- encIntSet'' btSize (pos-1) r_els
      mk var t1 t2
    where   (r_els,l_els) = IS.partition (\x -> testBit x pos) elems
            var = btSize - 1 - pos
{-# INLINE encIntSet'' #-}

tobdd :: [Int] -> ROBDD
tobdd = encIntSet . IS.fromList
list2bdd = showBDD . tobdd
list2bdd' = printBDD . encIntSet1 . IS.fromList

----
encInt :: Int -> ROBDD
encInt = encSingleton
encInt' = encIntSet . IS.singleton

encSingleton :: (Integral a, Hashable a, FiniteBits a) => a -> ROBDD
encSingleton (v :: a) = ROBDD (bddRevMap s) (bddIdSource s) bdd 
  where
    (bdd,s) = runBDDContext (encSingleton' (btS - 1)) emptyBDDState
    btS = finiteBitSize (undefined :: a)  
    encSingleton' :: Int -> BDDContext Int BDD BDD
    encSingleton' pos 
      | pos == -1 = return One
      | testBit v pos = do
         res <- encSingleton' (pos-1)
         mk (btS - 1 - pos) Zero res 
      | otherwise = do
         res <- encSingleton' (pos-1)
         mk (btS - 1 - pos) res Zero
{-# INLINE encSingleton #-}               

--decSingleton ::  (Ord a, FiniteBits a) => ROBDD -> a 
--decSingleton r = let (as :: (Ord a, FiniteBits a) => Set a)= decSet r in
--  if (Set.size as) == 1 
--  then Set.elemAt 0 as 
--  else (error "Not a singleton or Zero/One!") 

----
encSet :: (FiniteBits a, Ord a) => Set a -> ROBDD
encSet (ls :: Set a) = ROBDD (bddRevMap s) (bddIdSource s) bdd
  where
    (bdd, s) = runBDDContext (encSet' (btSize - 1) ls) emptyBDDState
    btSize = finiteBitSize (undefined :: a)
    encSet' :: (FiniteBits a, Ord a) => Int -> Set a -> BDDContext Int BDD BDD
    encSet' pos (elems :: Set a)
      | Set.null elems = return Zero
      | pos == -1 = return One
      | otherwise = do
        t1 <- encSet' (pos-1) l_els
        t2 <- encSet' (pos-1) r_els
        mk var t1 t2
       where  (r_els,l_els) = Set.partition (\x -> testBit x pos) elems
              var = finiteBitSize (undefined :: a) - 1 - pos


encHashSet :: (FiniteBits a,Hashable a,Ord a) => HashSet a -> ROBDD
encHashSet (ls :: HashSet a) = ROBDD (bddRevMap s) (bddIdSource s) bdd
  where
    (bdd, s) = runBDDContext (encHashSet' (btSize - 1) ls) emptyBDDState
    btSize = finiteBitSize (undefined :: a)
    encHashSet' :: (FiniteBits a,Hashable a,Ord a) => Int -> HashSet a -> BDDContext Int BDD BDD
    encHashSet' pos (elems :: HashSet a)
      | HS.null elems = return Zero
      | pos == -1 = return One
      | otherwise = do
        t1 <- encHashSet' (pos-1) l_els
        t2 <- encHashSet' (pos-1) r_els
        mk var t1 t2
       where   r_els = HS.filter (\x -> testBit x pos) elems
               l_els = HS.difference elems r_els
               var = finiteBitSize (undefined :: a) - 1 - pos
{-# INLINE encHashSet #-}

list2bdd'' :: [Int] -> ROBDD 
list2bdd'' = encHashSet . HS.fromList


---------
decIntSet :: ROBDD -> IntSet
decIntSet (ROBDD _ _ bdd) = decIntSet' (finiteBitSize p - 1) bdd 0 IS.empty
  where
    p = 0 :: Int
    decIntSet' :: Int -> BDD -> Int -> IntSet -> IntSet
    decIntSet' pos Zero value cur = cur
    decIntSet' pos One value cur = fillRes pos value cur
    decIntSet' pos node@(BDD l sym r _ _) value cur
      | sym < 0 = let s1 = decIntSet' pos r value cur
                      s2 = decIntSet' pos l value s1
                  in s2
      | sym > finiteBitSize value = fillRes pos value cur
      | (finiteBitSize value)-1-pos < sym  = let s1 = decIntSet' (pos-1) node (setBit value pos) cur
                                                 s2 = decIntSet' (pos-1) node value s1
                                       in s2
      | otherwise = let s1 = decIntSet' (pos-1) r (setBit value pos) cur
                        s2 = decIntSet' (pos-1) l value s1
                    in s2
    --
    fillRes :: Int -> Int -> IntSet -> IntSet
    fillRes pos value cur
      | pos == -1 = IS.insert value cur
      | otherwise = let s1 = fillRes (pos-1) (setBit value pos) cur
                        s2 = fillRes (pos-1) value s1
                    in s2
{-# INLINE decIntSet #-}


decSet :: (Ord a, FiniteBits a,Num a) => ROBDD -> Set a
decSet (ROBDD _ _ bdd) = let p = 0  
                         in decSet' (finiteBitSize (0 :: Int) - 1) bdd p Set.empty
  where
    decSet' :: (FiniteBits a, Ord a) => Int -> BDD -> a -> Set a -> Set a
    decSet' pos Zero value cur = cur
    decSet' pos One value cur = fillRes pos value cur
    decSet' pos node@(BDD l sym r _ _) value cur
      | sym < 0 = let s1 = decSet' pos r value cur
                      s2 = decSet' pos l value s1
                  in s2
      | sym > finiteBitSize value = fillRes pos value cur
      | (finiteBitSize value)-1-pos < sym  = let s1 = decSet' (pos-1) node (setBit value pos) cur
                                                 s2 = decSet' (pos-1) node value s1
                                       in s2
      | otherwise = let s1 = decSet' (pos-1) r (setBit value pos) cur
                        s2 = decSet' (pos-1) l value s1
                    in s2
    --
    fillRes :: (FiniteBits a,Ord a) => Int -> a -> Set a -> Set a
    fillRes pos value cur
      | pos == -1 = Set.insert value cur
      | otherwise = let s1 = fillRes (pos-1) (setBit value pos) cur
                        s2 = fillRes (pos-1) value s1
                    in s2
{-# INLINE decSet #-}
        

 
decHashSet :: (Ord a,Hashable a, FiniteBits a,Num a) => ROBDD -> HashSet a
decHashSet (ROBDD _ _ bdd) = let p = 0  
                             in decHashSet' (finiteBitSize (0 :: Int) - 1) bdd p HS.empty
  where 
    decHashSet' :: (FiniteBits a,Hashable a, Ord a) => Int -> BDD -> a -> HashSet a -> HashSet a
    decHashSet' pos Zero value cur = cur
    decHashSet' pos One value cur = fillRes pos value cur
    decHashSet' pos node@(BDD l sym r _ _) value cur
      | sym < 0 = let s1 = decHashSet' pos r value cur
                      s2 = decHashSet' pos l value s1
                  in s2
      | sym > finiteBitSize value = fillRes pos value cur
      | (finiteBitSize value)-1-pos < sym  = let s1 = decHashSet' (pos-1) node (setBit value pos) cur
                                                 s2 = decHashSet' (pos-1) node value s1
                                       in s2
      | otherwise = let s1 = decHashSet' (pos-1) r (setBit value pos) cur
                        s2 = decHashSet' (pos-1) l value s1
                    in s2
    --
    fillRes :: (FiniteBits a,Hashable a,Ord a) => Int -> a -> HashSet a -> HashSet a
    fillRes pos value cur
      | pos == -1 = HS.insert value cur
      | otherwise = let s1 = fillRes (pos-1) (setBit value pos) cur
                        s2 = fillRes (pos-1) value s1
                    in s2
{-# INLINE decHashSet #-} 

{-
---- for Integer
encSignedRange :: (Ord a,FiniteBits a) => Int -> a -> a -> ROBDD
encSignedRange off l u
    | l >= 0 = encodeRange off l u
    | u < 0  = encodeRange off l u
    | otherwise = do
        ln <- encodeRange off l (-1)
        rn <- encodeRange off 0 u
        ln #|| rn

encodeRange :: (Ord a,Monad m,FiniteBits a) => Int -> a -> a -> BDDM s Int m (Tree s Int)
encodeRange off l u
    | l <= u = encodeRange' l u ((finiteBitSize l) - 1)
    | otherwise = error "Lower bound is greater than upper bound"
      where
        encodeRange' :: (Monad m,FiniteBits a) => a -> a -> Int -> BDDM s Int m (Tree s Int)
        encodeRange' l u pos
          | pos == -1 = true
          | (testBit l pos) && (testBit u pos) = do
            ln <- encodeRange' l u (pos-1)
            rn <- false
            node n ln rn
          | (not $ testBit l pos) && (not $ testBit u pos) = do
            ln <- false
            rn <- encodeRange' l u (pos-1)
            node n ln rn
          | otherwise = do
            ln <- encodeRangeU' u (pos-1)
            rn <- encodeRangeL' l (pos-1)
            node n ln rn
            where
              n = finiteBitSize l - pos + off - 1

        encodeRangeL' :: (Monad m,FiniteBits a) => a -> Int -> BDDM s Int m (Tree s Int)
        encodeRangeL' l pos
          | pos == -1 = true
          | testBit l pos = do
            ln <- encodeRangeL' l (pos-1)
            rn <- false
            node n ln rn
          | otherwise = do
            ln <- true
            rn <- encodeRangeL' l (pos-1)
            node n ln rn
            where
              n = finiteBitSize l - pos + off - 1

        encodeRangeU' :: (Monad m,FiniteBits a) => a -> Int -> BDDM s Int m (Tree s Int)
        encodeRangeU' u pos
          | pos == -1 = true
          | testBit u pos = do
            ln <- encodeRangeU' u (pos-1)
            rn <- true
            node n ln rn
          | otherwise = do
            ln <- false
            rn <- encodeRangeU' u (pos-1)
            node n ln rn
            where
              n = finiteBitSize u - pos + off - 1
-}



----------------------
---
mrgBDD :: (BDDable a, BDDable b) => a -> b -> ROBDD
mrgBDD a b = bddUnion (toBDD a) (toBDD b)

mrgBDDs = bddUnions . map toBDD

bddUnion = or
bddUnions :: [ROBDD] -> ROBDD
bddUnions = foldl' or makeFalse  

elemBDD = bddElem
bddElem :: (BDDable a, BDDable b) => a -> b -> Bool
bddElem r1 r2 = and r1_bdd (toBDD r2) == r1_bdd
   where r1_bdd = toBDD r1

----
mrgByBDD :: (BDDable a) => [a] -> IntSet
mrgByBDD = fromBDD . mrgBDDs

mrgISByBDD :: IntSet -> IntSet -> IntSet
mrgISByBDD a b = mrgByBDD [a,b]


-------------------------------------------
getBDD :: ROBDD -> BDD
getBDD (ROBDD _ _ bdd) = bdd

toROBDD :: BDD -> ROBDD
toROBDD bdd = ROBDD HM.empty [0..] bdd

------- Serialization for ROBDD ----------- 
instance Binary ROBDD where
  put = serializeBDD . getBDD              -- serializeROBDD
  get = liftM toROBDD deserializeBDD      -- deserializeROBDD

instance Binary BDD where
  put = serializeBDD
  get = deserializeBDD
    
-- | Use the binary library to serialize a ROBDD.
serializeROBDD :: ROBDD -> Put
serializeROBDD (ROBDD rm s bdd) = put rm >> put (head s) >> put loc >> act  
  where
    (loc,act,m,p) = serialize IM.empty 0 bdd

deserializeROBDD :: Get ROBDD
deserializeROBDD = do
  rm <- get
  maxId <- get
  loc <- get
  (bdd,_,_) <- deserialize IM.empty 0 loc
  return $ ROBDD rm (drop maxId [0..]) bdd

serializeBDD :: BDD -> Put
serializeBDD bdd = put loc >> act  
  where
    (loc,act,m,p) = serialize IM.empty 0 bdd

deserializeBDD :: Get BDD
deserializeBDD = do
  loc <- get
  (bdd,_,_) <- deserialize IM.empty 0 loc
  return bdd
    
serialize :: IntMap Int -> Int -> BDD -> (Int,Put,IntMap Int,Int)
serialize mp pos Zero = (-2,return (), mp, pos)
serialize mp pos One  = (-1,return (), mp, pos)
serialize mp pos (BDD l val r un _) = case IM.lookup un mp of
   Just loc -> (loc,return (),mp,pos)  
   Nothing -> let (lloc,lact,nmp1,npos1) = serialize (IM.insert un pos mp) (pos+1) l
                  (rloc,ract,nmp2,npos2) = serialize nmp1 npos1 r
              in (pos,
                  put val >> put lloc >> put rloc >> lact >> ract,
                  nmp2, npos2) --  `debug` (show lloc ++ ", " ++ show rloc) 
    
deserialize :: IntMap BDD -> Int -> Int -> Get (BDD,IntMap BDD,Int)
deserialize mp pos loc = case loc of
    -2 -> return (Zero,mp,pos)
    -1 -> return (One, mp,pos)       
    _  -> case IM.lookup loc mp of
      Just bdd -> return (bdd,mp,pos)
      Nothing ->  do  -- error $ "Internal deserialization error at "++show loc++show (IM.keys mp)         
           val <- get
           lloc <- get
           rloc <- get
           (l,nmp1,npos1) <- deserialize mp (pos+1) lloc
           (r,nmp2,npos2) <- deserialize nmp1 npos1 rloc
           let bdd = BDD l val r npos2 (hashNode val l r)
           return (bdd,IM.insert pos bdd nmp2,npos2)

-- encode = runPut. Put;  decode = runGet get
--encodeBDD :: BDD -> BS.ByteString
encodeBDD = runPut . serializeBDD    
decodeBDD = runGet deserializeBDD


-- encodeFile f = L.writeFile f . encode;  decodeFile :: FilePath -> IO a
writeBDD :: FilePath -> ROBDD -> IO ()
writeBDD = encodeFile 
readBDD :: FilePath -> IO ROBDD
readBDD = decodeFile

-- | Cache a BDD in a file. If the file exists, the BDD is loaded from it. If it doesn't, the BDD is built
--   and stored into the file.
cachedBDD :: FilePath  -> IO ROBDD -> IO ROBDD
cachedBDD fp act = do
  exists <- doesFileExist fp
  if exists  then readBDD fp
  else do  bdd <- act
           writeBDD fp bdd
           return bdd
 

----------------------------
--- Util functions ---------           
getSize :: (Integral a) => a -> Int
getSize v = if lenSize <= pcSize then lenSize 
            else (error "The biggest member is too large!")
  where  
    pcSize = finiteBitSize (undefined :: Int)
    lenSize = ceiling . logBase 2 . fromInteger . toInteger $ v + 1

isOverflow :: (Integral a) => a -> Bool
isOverflow v = lenSize > pcSize 
  where  
    pcSize = finiteBitSize (undefined :: Int)
    lenSize = ceiling . logBase 2 . fromInteger . toInteger $ v + 1

toInt :: Integer -> Int
toInt k = if k >= toInteger (minBound :: Int) && k <= toInteger (maxBound :: Int) 
          then (fromInteger k :: Int)
          else error $ "The number " ++ show k ++ " is out of the Int bound!"

-----------------------------    
showBDD :: ROBDD -> IO ()
showBDD (ROBDD m s bdd) = putStrLn $
   "\tRevMap= " ++ show m ++ "\n\tMaxId= " ++ show (head s) ++ "\n\tOBDD = " ++ showBD bdd   
  where
    showBD Zero = " Zero "
    showBD One  = " One "
    showBD (BDD l v h i _) = " BDD(" ++ showBD l ++ show v ++ showBD h ++ show i ++ ") "

printBDD :: ROBDD -> IO ()
printBDD bdd = do
  let dag = makeDAG bdd
      params = nonClusteredParams { fmtNode = \(_,l) -> [toLabel l]
                                  , fmtEdge = \(_,_,l) -> [toLabel l]
                                  }
      dg = graphToDot params dag 
  runGraphviz dg Png "BDDtemp.png" 
  system "BDDtemp.png"  
  return ()
  
  
--------------- Test
test,test1,test2,test3,test3' :: [Int] -> IntSet 
test = fromBDD . toBDD
test1 = decIntSet . encHashSet . HS.fromList 
test2 = decIntSet . encSet . Set.fromList

test3 = fromBDD . toROBDD. decodeBDD. encodeBDD. getBDD. toBDD
test3' = fromBDD. decode. encode. toBDD

test' :: (FiniteBits a,Ord a,Num a,Hashable a) => [a] -> HashSet a
test' = decHashSet . encHashSet . HS.fromList 

test4 :: [Int] -> IO ()
test4 ls = do 
    writeBDD "tmp.robdd" (toBDD ls)
    rb <- readBDD "tmp.robdd" 
    print (fromBDD rb :: IntSet)

{-
-- test with memcached   
instance MC.Value LS.ByteString where
  serializeValue = lazyToStrict
  deserializeValue = Right . strictToLazy

strictToLazy :: BS.ByteString -> LS.ByteString
strictToLazy x
  | BS.null x = LS.Empty
  | otherwise = LS.Chunk x LS.Empty

lazyToStrict :: LS.ByteString -> BS.ByteString
lazyToStrict = BS.concat . LS.toChunks
{-# INLINE lazyToStrict #-}  

test5 = do
  mValue <- MC.withClient "127.0.0.1:11211" action
  case mValue of
    Nothing -> putStrLn "(no value)"
    Just value -> print (fromBDD $ decode value :: IntSet)
 where
   action client = do 
     MC.set client "k10"  (encode $ tobdd [1..10] )
     MC.get client "k10"

test6 :: IO ()
test6 = runResourceT $ do
  (rkeyClient , client) <- flip allocate MC.closeClient $ do
    c <- MC.openClient "127.0.0.1:11211"
    case c of
      Just client -> return (client)
      Nothing -> liftIO $ do
        throwIO (userError "could not open.")
  liftIO $ do
    ret <- MC.set client "key" "hello"
    print ret
    ret' <- MC.get client "key" :: IO (Maybe String)
    print ret'
  release rkeyClient 
-}