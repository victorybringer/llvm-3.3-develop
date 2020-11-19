{-# LANGUAGE ScopedTypeVariables,BangPatterns,NoMonomorphismRestriction,TypeSynonymInstances,
             FlexibleInstances,DeriveGeneric,UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module LLVM.Slicing.Data.HBDD (
 BDDable(..), 
 CxtAndBDD, BDDCxt, BDD, BDDSt, BDDWithCxt,
 runBDDSt, getBDD, getBDDCxt, withCxt,
 
 mrgBDD, mrgBDDs, mrgByBDD, mrgByBDDWith,
 mrgIntSet, mrgIntSets, mrgISWith, 
 allToBDD, mrgInts,
 
 showbdd, showbdd', printBDD, printBDD',
 
 module Data.HBDD.ROBDDContext,
 module Data.HBDD.ROBDD,
 module Data.HBDD.Operations,
 module Data.HBDD.ROBDDFactory,
 module Data.HBDD.ROBDDDot,
 module Data.HBDD.ROBDDState
 )where

--import System.Environment
import System.FilePath
import System.Process
import System.Directory (doesFileExist)
import qualified Control.Monad.Trans.State.Strict as ST
import Prelude hiding(and,or,not)
import qualified Prelude as P (and,or,not)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits
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
import Data.Hashable (Hashable)
import Data.List (sortBy,foldl')
import Data.Function (on)
import Data.Monoid 
import Control.Monad (liftM)

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import GHC.Generics (Generic)
import Control.DeepSeq

import Data.HBDD.ROBDDContext
import Data.HBDD.ROBDDState
import Data.HBDD.ROBDD 
import Data.HBDD.Operations -- as BDD
import Data.HBDD.ROBDDDot
import Data.HBDD.ROBDDFactory
import Data.HBDD.UIDGenerator hiding (allocId)
import qualified Data.HBDD.UIDGenerator as UIDG (allocId)


type BDDWithCxt = (BDD, BDDCxt)
type BDDCxt = ROBDDContext Int
type BDD = ROBDD Int
type BDDSt = ROBDDState Int

runBDDSt :: BDDCxt -> BDDSt -> BDDWithCxt
runBDDSt = flip ST.runState
getBDD :: BDDCxt -> BDDSt -> BDD 
getBDD = flip ST.evalState
getBDDCxt :: BDDCxt -> BDDSt -> BDDCxt 
getBDDCxt = flip ST.execState
--
withCxt :: (BDDCxt -> BDDCxt) -> BDDSt -> BDDSt
withCxt = ST.withState

--mrgBDD :: ROBDDBinOp a b Int => a -> b -> BDDSt
mrgBDD = orC

mrgBDDs :: [BDDSt] -> BDDSt
mrgBDDs = foldBySizeC orC

allToBDD :: BDDable a => BDDCxt -> [a] -> BDDWithCxt
allToBDD cxt = runBDDSt cxt . mrgBDDs . map toBDDSt

mrgByBDD :: (BDDable c, BDDable a) => [a] -> c
mrgByBDD = mrgByBDDWith mkContext

mrgByBDDWith :: (BDDable c, BDDable a) => BDDCxt -> [a] -> c
mrgByBDDWith cxt = fromBDDSt cxt . mrgBDDs . map toBDDSt
{-# INLINE mrgByBDDWith #-}

mrgIntSets :: [IntSet] -> IntSet
mrgIntSets = mrgByBDD 

mrgIntSet :: IntSet -> IntSet -> IntSet
mrgIntSet = mrgISWith mkContext 

mrgISWith :: BDDCxt -> IntSet -> IntSet -> IntSet
mrgISWith cxt a b = fromBDDSt cxt $ mrgBDD (toBDDSt a) (toBDDSt b)
{-# INLINE mrgIntSet #-}

mrgInts :: [[Int]] -> IntSet
mrgInts = mrgByBDD . map IS.fromList

--------------------------------------------
class BDDable a where
  toBDD :: BDDCxt -> a -> CxtAndBDD Int  
  fromBDD :: CxtAndBDD Int -> a 
  
  toBDD' :: a -> BDD
  toBDD' = getBDD mkContext . toBDDSt
  
  toBDDSt :: a -> BDDSt
  toBDDSt v = do 
      cxt <- ST.get
      let (cxt',bdd) = toBDD cxt v
      ST.put $! clearOpContext cxt'
      return $! bdd 
  {-# INLINE toBDDSt #-}
      
  fromBDDSt :: BDDCxt -> BDDSt -> a 
  fromBDDSt cxt bst = fromBDD (cxt',bdd)
    where  (bdd,cxt') = runBDDSt cxt bst   
  {-# INLINE fromBDDSt #-}

instance BDDable Int where
  toBDD = encInt 
  fromBDD r =  let ls = decIntSet r in
      if IS.size ls == 1 
      then head (IS.elems ls) 
      else (error "Not a singleton Int, or a Zero/One!")

instance BDDable Integer where
  toBDD cxt = encInt cxt. toInt
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

--instance (Ord a, FiniteBits a,Num a) => BDDable a where
--  toBDD cxt = encSet cxt. Set.singleton
--  fromBDD r = let (as :: Set a)= decSet r in
--     if (Set.size as) == 1 
--     then Set.elemAt 0 as 
--     else (error "Not a singleton or Zero/One!") 

instance (Ord a, FiniteBits a, Num a) => BDDable [a] where
  toBDD cxt = encSet cxt. Set.fromList
  fromBDD = Set.toList . decSet

instance BDDable [Int] where
  toBDD cxt = encIntSet cxt. IS.fromList
  fromBDD = IS.toList . decIntSet

instance {-# OVERLAPS #-} BDDable [Integer] where     --  
  toBDD cxt = encIntSet cxt. IS.fromList . map toInt
  fromBDD = map toInteger . IS.toList . decIntSet


-----
instance Show v => Show (ROBDDContext v) where
  show (ROBDDContext _ iam im) = 
      " BDD_Cache:" ++ show iam ++ "\n BDDOp_Cache:" ++ show im ++ "\n"
      
instance Eq v => Eq (ROBDDContext v) where
  (ROBDDContext _ iam1 im1) == (ROBDDContext _ iam2 im2) = iam1 == iam2
--      iam1 == iam2 && im1 == im2

instance Ord v => Ord (ROBDDContext v) where
  compare r1 r2 = compare (fst $ allocId r1) (fst $ allocId r2)
      
instance (Ord v) => Monoid (ROBDDContext v) where
  mempty = mkContext
  mappend r1@(ROBDDContext g1 nc1 oc1) r2@(ROBDDContext g2 nc2 oc2) =
      if r1 > r2 then r1 else r2 
--      ROBDDContext g' (mappend nc1 nc2) (mappend oc1 oc2)
--    where  g' = if fst (allocId r1) > fst (allocId r2) then g1 else g2 
  
instance (Ord v) => NFData (ROBDDContext v) where
  rnf (ROBDDContext _ !nc _) = ()  
       
instance (Ord v, NFData v) => Ord (ROBDD v) where
  compare (ROBDD _ _ _ i1 _) (ROBDD _ _ _ i2 _) = compare i1 i2
 
-----
encIntSet :: BDDCxt -> IntSet -> (BDDCxt, BDD)
encIntSet = encIntSet' (finiteBitSize (undefined :: Int))
{-# INLINE encIntSet #-}

encIntSet1 cxt s = encIntSet' (getSize $ IS.findMax s) cxt s

encIntSet' btSize = encIntSet'' btSize (btSize - 1)    

encIntSet'' :: Int -> Int -> BDDCxt -> IntSet -> (BDDCxt, BDD)
encIntSet'' btSize pos cxt elems
  | IS.null elems = (cxt,Zero)
  | pos == -1 = (cxt,One)
  | otherwise = mkNode cxt2 t1 var t2
    where   (r_els,l_els) = IS.partition (\x -> testBit x pos) elems
            (cxt1, t1) = encIntSet'' btSize (pos-1) cxt l_els
            (cxt2, t2) = encIntSet'' btSize (pos-1) cxt1 r_els
            var = btSize - 1 - pos
{-# INLINE encIntSet'' #-}

tobdd = encIntSet mkContext . IS.fromList
list2bdd = snd . tobdd 
showbdd = printBDD' . list2bdd
showbdd4 = printBDD . encIntSet' 4 mkContext . IS.fromList

----
encInt :: BDDCxt -> Int -> (BDDCxt, BDD)
encInt = encSingleton

encInt' cxt = encIntSet cxt . IS.fromList . (:[])

encSingleton :: (Integral a, FiniteBits a) => BDDCxt -> a -> (BDDCxt, BDD)
encSingleton context v = encSingleton' (btS - 1) context
  where
    btS = finiteBitSize v   -- getSize v 
    encSingleton' pos cxt
      | pos == -1 = (cxt,One)
      | testBit v pos = mkNode cxt' Zero (btS - 1 - pos) res 
      | otherwise = mkNode cxt' res (btS - 1 - pos) Zero
      where (cxt', res) = encSingleton' (pos-1) cxt      
{-# INLINE encSingleton #-}               

----
encSet :: (FiniteBits a, Ord a) => BDDCxt -> Set a -> (BDDCxt, BDD)
encSet context (set :: Set a) = encSet' (finiteBitSize (undefined :: a)- 1) context set
  where
    encSet' :: (FiniteBits a, Ord a) => Int -> BDDCxt
                   -> Set a -> (BDDCxt, BDD)
    encSet' pos cxt (elems :: Set a)
      | Set.null elems = (cxt,Zero)
      | pos == -1 = (cxt,One)
      | otherwise = mkNode cxt2 t1 var t2
        where   (r_els,l_els) = Set.partition (\x -> testBit x pos) elems
                (cxt1, t1) = encSet' (pos-1) cxt l_els
                (cxt2, t2) = encSet' (pos-1) cxt1 r_els
                var = finiteBitSize (undefined :: a) - 1 - pos

encHashSet :: (FiniteBits a,Hashable a,Ord a) => BDDCxt -> HashSet a -> (BDDCxt, BDD)
encHashSet context (set :: HashSet a) = encHashSet' (finiteBitSize (undefined :: a)- 1) context set
  where
    encHashSet' :: (FiniteBits a,Hashable a,Ord a) => Int -> BDDCxt
                   -> HashSet a -> (BDDCxt, BDD)
    encHashSet' pos cxt (elems :: HashSet a)
      | HS.null elems = (cxt,Zero)
      | pos == -1 = (cxt,One)
      | otherwise = mkNode cxt2 t1 var t2
        where   r_els = HS.filter (\x -> testBit x pos) elems
                l_els = HS.difference elems r_els
                (cxt1, t1) = encHashSet' (pos-1) cxt l_els
                (cxt2, t2) = encHashSet' (pos-1) cxt1 r_els
                var = finiteBitSize (undefined :: a) - 1 - pos
{-# INLINE encHashSet #-}

list2bdd' :: [Int] -> BDD
list2bdd' = snd . encHashSet mkContext . HS.fromList
showbdd' :: [Int] -> IO ()
showbdd' = printBDD' . list2bdd' 


---------
decIntSet :: (BDDCxt, BDD) -> IntSet
decIntSet (context, bdd) = let p = 0 :: Int
                           in decIntSet' (finiteBitSize p - 1) bdd p IS.empty
  where
    decIntSet' :: Int -> BDD -> Int -> IntSet -> IntSet
    decIntSet' pos Zero value cur = cur
    decIntSet' pos One value cur = fillRes pos value cur
    decIntSet' pos (ROBDDRef l v r _ _) value cur = 
      decIntSet' pos (lookupUnsafe (ROBDDId l v r) context) value cur
    decIntSet' pos node@(ROBDD l sym r _ _) value cur
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

test,test1,test2 :: [Int] -> IntSet 
test = decIntSet . encIntSet mkContext . IS.fromList
test1 = decIntSet . encHashSet mkContext . HS.fromList 
test2 = decIntSet . encSet mkContext . Set.fromList  


decSet :: (FiniteBits a,Ord a,Num a) => (BDDCxt, BDD) -> Set a
decSet (context, bdd) = let p = 0  
                        in decSet' (finiteBitSize (p::Int) - 1) bdd p Set.empty
  where
    decSet' :: (FiniteBits a, Ord a) => Int -> BDD -> a -> Set a -> Set a
    decSet' pos Zero value cur = cur
    decSet' pos One value cur = fillRes pos value cur
    decSet' pos (ROBDDRef l v r _ _) value cur = 
      decSet' pos (lookupUnsafe (ROBDDId l v r) context) value cur
    decSet' pos node@(ROBDD l sym r _ _) value cur
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

test' :: (FiniteBits a,Ord a,Num a) => [a] -> Set a 
test' = decSet . encSet mkContext . Set.fromList           

decHashSet :: (FiniteBits a,Ord a,Hashable a,Num a) => (BDDCxt, BDD) -> HashSet a
decHashSet (context, bdd) = let p = 0  
                        in decHashSet' (finiteBitSize (p::Int) - 1) bdd p HS.empty
  where
    decHashSet' :: (FiniteBits a, Ord a,Hashable a) => Int -> BDD -> a -> HashSet a -> HashSet a
    decHashSet' pos Zero value cur = cur
    decHashSet' pos One value cur = fillRes pos value cur
    decHashSet' pos (ROBDDRef l v r _ _) value cur = 
      decHashSet' pos (lookupUnsafe (ROBDDId l v r) context) value cur
    decHashSet' pos node@(ROBDD l sym r _ _) value cur
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
    fillRes :: (FiniteBits a,Ord a,Hashable a) => Int -> a -> HashSet a -> HashSet a
    fillRes pos value cur
      | pos == -1 = HS.insert value cur
      | otherwise = let s1 = fillRes (pos-1) (setBit value pos) cur
                        s2 = fillRes (pos-1) value s1
                    in s2
{-# INLINE decHashSet #-}

----------------------
encIntSets :: BDDCxt -> [IntSet] -> (BDDCxt, BDD)
encIntSets cxt vs = swap $ allToBDD cxt vs    

mixUnion :: [Int] -> (BDDCxt,BDD) -> (BDDCxt,BDD)
mixUnion ls (cxt,bdd) = or cxt' b bdd 
  where (cxt', b) = encIntSet cxt $ IS.fromList ls 

tobdds = encIntSets mkContext . map IS.fromList
lists2bdd = snd . tobdds 
listUnions = decIntSet . tobdds 


-----------------
---
bddUnions :: Ord v => ROBDDContext v -> [ROBDD v] -> CxtAndBDD v
bddUnions = foldBySize or  
 
bddUnions' :: Ord v => [CxtAndBDD v] -> CxtAndBDD v
bddUnions' = foldr or' (mkContext,Zero)             -- foldl1 or' 
ors = bddUnions'

bddUnions2 :: Ord v => [ROBDDState v] -> ROBDDState v
bddUnions2 = foldBySizeC orC

bddUnion :: Ord v => CxtAndBDD v -> CxtAndBDD v -> CxtAndBDD v
bddUnion cb1 cb2 =   -- or' 
  if (fst cb1 > fst cb2) 
  then orWithCxt (fst cb1) cb1 cb2     -- or' cb2 cb1
  else orWithCxt (fst cb2) cb1 cb2     -- or' cb1 cb2

orWithCxt :: Ord v => ROBDDContext v -> CxtAndBDD v -> CxtAndBDD v -> CxtAndBDD v
orWithCxt cxt cb1 cb2 = apply2 (||) cb1 cb2 cxt

or' :: Ord v => CxtAndBDD v -> CxtAndBDD v -> CxtAndBDD v
or' = apply' (||)
xor' = apply' (/=)

apply' fn (cxt1,b1) (cxt2,b2) 
  | cxt1 == cxt2 = apply fn cxt2 b1 b2
  | otherwise = apply2 fn (cxt1,b1) (cxt2,b2) cxt2  -- mkContext
{-# INLINE apply' #-}

------------------------------------------
type BDDOp v = ROBDDContext v -> ROBDD v -> ROBDD v -> CxtAndBDD v
type CxtAndBDD v = (ROBDDContext v, ROBDD v)

foldBySize :: Ord v => BDDOp v -> ROBDDContext v -> [ROBDD v] -> CxtAndBDD v
foldBySize f context l = go context Zero sorted
  where
    go cxt b [] = (cxt,b)
    go cxt b (x:xs) = let (cxt',b') = f cxt b x
                          cxt2 = clearOpContext cxt'
                      in b' `seq` go cxt2 b' xs
    sorted = sortBy (compare `on` size) l
{-# INLINE foldBySize #-} 

apply2 :: Ord v => BinOp -> CxtAndBDD v -> CxtAndBDD v -> ROBDDContext v -> CxtAndBDD v
apply2 fn (cxt, lt@(ROBDD left var right _ _))(cxt', rt@(ROBDD left' var' right' _ _)) ct =   
  let opId = ROBDDOpId (identifier lt) (identifier rt) in
  case lookupOp opId ct of
   Just o  -> (ct, o)
   Nothing -> let (ct', res) = case compare var var' of
                              EQ -> applyRec2 fn cxt var left right cxt' left' right' ct
                              LT -> applyRec2 fn cxt var left right cxt' rt rt ct 
                              GT -> applyRec2 fn cxt var' lt lt cxt' left' right' ct
              in  (insertOp opId res ct', res)
apply2 fn (cxt,(ROBDDRef left var right _ _)) (cxt',rightTree) ct =
  apply2 fn (cxt,lookupUnsafe (ROBDDId left var right) cxt) (cxt',rightTree) ct
apply2 fn (cxt,leftTree) (cxt',ROBDDRef left var right _ _) ct =
  apply2 fn (cxt,leftTree) (cxt',lookupUnsafe (ROBDDId left var right) cxt') ct
apply2 fn (cxt,Zero) (cxt',ROBDD left var right _ _) ct =
  applyRec2 fn cxt var Zero Zero cxt' left right ct
apply2 fn (cxt,One) (cxt',ROBDD left var right _ _) ct =
  applyRec2 fn cxt var One One cxt' left right ct
apply2 fn (cxt,ROBDD left var right _ _) (cxt',Zero) ct =
  applyRec2 fn cxt var left right cxt' Zero Zero ct
apply2 fn (cxt,ROBDD left var right _ _) (cxt',One) ct =
  applyRec2 fn cxt var left right cxt' One One ct
apply2 fn (cxt,a) (cxt',b) ct =
  (ct, boolToLeaf $ leafToBool a `fn` leafToBool b)
{-# INLINE apply2 #-}

applyRec2 :: Ord v => BinOp -> ROBDDContext v -> v -> ROBDD v -> ROBDD v -> 
         ROBDDContext v -> ROBDD v -> ROBDD v -> ROBDDContext v -> CxtAndBDD v
applyRec2 fn cxt var left right cxt' left' right' ct = 
    let (leftCxt, resLeft)   = apply2 fn (cxt,left) (cxt',left') ct
        (rightCxt, resRight) = apply2 fn (cxt,right) (cxt',right') leftCxt
    in mkNode rightCxt resLeft var resRight

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

-----    
printBDD (cxt,b) = writeGr "BDD.dot" (showDot cxt b)

printBDD' = writeGr "BDD.dot" . showDot2
   
writeGr filename contents = do
   writeFile filename contents 
   system $ "dot -Tpng " ++ filename  ++ " -o " ++ (filename ++ ".png")
   system $ filename ++ ".png"
   return ()

swap (a,b) = (b,a)

---
setId :: Int -> ROBDDContext v -> ROBDDContext v
setId n (ROBDDContext ug nc oc) =  ROBDDContext (setId' n ug) nc oc
setIds ls (ROBDDContext ug nc oc) =  ROBDDContext (setIds' ls ug) nc oc

getId (ROBDDContext ug _ _) = getId' ug
getIds (ROBDDContext ug _ _) = getIds' ug

setId' :: Int -> UIDGenerator -> UIDGenerator
setId' n u@(UIDGenerator []) = u
setId' n (UIDGenerator ids) = UIDGenerator $ drop (n - head ids) ids

setIds' :: [Int] -> UIDGenerator -> UIDGenerator
setIds' ls (UIDGenerator _) = UIDGenerator ls

allocId' = UIDG.allocId
getId' = fst . UIDG.allocId 
getIds' (UIDGenerator ids) = ids



{- -- some test results
 > snd . encIntSet' 4 mkContext . IS.fromList $ [0,1,3,4,9,10,11,12]
  ROBDD (ROBDD (ROBDD One 2 (ROBDD Zero 3 One 2 3) 3 5) 1 
          (ROBDD (ROBDD One 3 Zero 4 3) 2 Zero 5 5) 6 11) 
        0 
        (ROBDD (ROBDD (ROBDDRef 0 3 1 2 3) 2 One 7 5) 1 
          (ROBDDRef 4 2 0 5 5) 8 11) 9 23  
-}  


mytest = do
-- printBDD' bd1
-- printBDD' bd2
 print $ decIntSet (cxt'',bd'')
-- printBDD (BDD.and cxt2 bd1 bd2)
 where
--  (cxt1, bd1) = encIntSet' 4 mkContext $ IS.fromList [0,1,2,3,4,9,10,11,12]
--  (cxt2, bd2) = encIntSet' 4 cxt1 $ IS.fromList [0,1,2,3,4,8,9,10]
  (cxt1, bd1) = encIntSet mkContext $ IS.fromList [0,(-1),2,(-3),4,9,(-10),11,(-12)]
  (cxt2, bd2) = encIntSet cxt1 $ IS.fromList [100..110]
  (cxt1', bd1') = encIntSet mkContext $ IS.fromList [0,1,2,3,4,9,10,11,12]
  (cxt2', bd2') = encIntSet cxt1' $ IS.fromList [5,8,9,10,15]
  (cxt,bd) = or cxt2' bd1' bd2'   
  (cxt',bd') = or' (cxt,bd) (cxt2,bd2)
  (cxt'',bd'') = ors [(cxt1, bd1),(cxt2, bd2),(cxt1', bd1'),(cxt',bd')]  
       