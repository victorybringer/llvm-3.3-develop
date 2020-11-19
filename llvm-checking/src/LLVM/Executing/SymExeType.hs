{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}


module LLVM.Executing.SymExeType ( 
  -- * Types
  SymExecutor, SymExeExpr, VarValueTable, Constraint,
  SymExeEnv(..), SymExeInfo(..),
  GlobalSbvArgs, FormalSbvArgs, GlbFrmlArgs,
  SymExeSummary(..), VarTableFunc, UnsignedVarTbl,
  -- * Constructor
  runSymExecutor, symExeEnvironment, 
  symExeLocal, getSymExeEnv,
  top, meetSymExeInfo, 
  varValueTbl, varValueSummary, paraInitSummary,
  findVarWith, findIdWith, findVar, findId,
  lkpValStr, updValStr, lkpValId, updValId,
  lkpVar, updVar, updVar2, updVars,
  xtdVar, xtdVars, xtdInst,
  lkpInst, updInst, lkpInst', updInst',
  lkpCond, updCond, lkpCond', updCond', updConds,
  -- * Utils
  sbvToSBool, toSBool, toSBool2, 
  isSBVTrue, sbvAsBool, sbvToBool, toBool, 
  isSat, querySat, chkSatWith, isOverflow,
  satSbvBool, satBool, satSBool,
  nilSBV, nilSExpr, mrgSExpr,  
  zeroSExpr, oneSExpr, nullSExpr, 
  trueSExpr, falseSExpr,
  unsafeCastSBV, toSList, 
  forceToIntSbv, isFpDblValue,
  fromSBool, fromDouble, fromFloat,
  toSExpr, toSExpr', intToSExpr, toSInt32,
  anyEqSExpr, allEqSExpr
  )
 where

import           Control.DeepSeq
import           Control.Lens  ( makeLenses, (%~) )
import           Control.Monad.Reader
import           GHC.Generics    ( Generic ) 
import           System.IO.Unsafe ( unsafePerformIO )
import           Data.IORef 

import           Data.List    (foldl') 
import           Data.Maybe   (fromMaybe, mapMaybe)
import           Data.Set ( Set )
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.Map (Map)
import qualified Data.Map.Strict as M
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup (Semigroup(..))
#endif
-- import           Data.Sequence  (Seq, empty, (|>))

import           Data.SBV 
import           Data.SBV.Dynamic ( svAsBool )
import qualified Data.SBV.List as S  ( nil )
import           Data.SBV.Internals  ( SBV(..), SV(..), SVal(..) )
import           Data.SBV.Control 

--
import           LLVM.Analysis   (Value, IsValue, Instruction(..), valueUniqueId)
import           LLVM.Analysis.CDG   (CDG)
import           LLVM.Analysis.Dominance  (DominatorTree)
import           LLVM.Slicing.Util.Utils (ValueId, ValueIds, toVarName')

import           LLVM.Checking.Inference.IndirectCallResolver (IndirectCallSummary)
import           LLVM.Checking.Inference.Output (OutputSummary)

----
type SymExecutor = ReaderT SymExeEnv Symbolic

runSymExecutor :: SymExecutor a -> SymExeEnv -> a 
runSymExecutor se env = unsafePerformIO . runSMT $ runReaderT se env

symExeEnvironment :: (SymExeEnv -> a) -> SymExecutor a
symExeEnvironment = asks

symExeLocal :: (SymExeEnv -> SymExeEnv) -> SymExecutor a -> SymExecutor a
symExeLocal = local 

getSymExeEnv :: SymExecutor SymExeEnv
getSymExeEnv = ask

-- getSymExeState :: SymExecutor State
-- getSymExeState = symbolicEnv


-----
type Name = String
type Constraint = SBool
type SymExeExpr = Set SInt32     -- SList Integer    -- SBV [Int32]
type VarValueTable = Map Name SymExeExpr
type VarTableFunc = [SymExeExpr] -> VarValueTable
type UnsignedVarTbl = Map Name (IntMap String)

instance {-# OVERLAPPING #-} Eq SInt32 where
  sa == sb = sbvToBool $ sa .== sb

instance Ord SInt32 where
  sa <= sb = sbvToBool $ sa .<= sb

instance {-# OVERLAPPING #-} Eq SymExeExpr where
  sa == sb =  sbvToBool (sa .== sb)     -- (S.size sa > 30 && S.size sb > 30)

instance {-# OVERLAPPING #-} Ord SymExeExpr where
  sa <= sb = S.size sa <=  S.size sb
  
instance {-# OVERLAPPING #-} Eq Constraint where
  sa == sb = sbvToBool sa == sbvToBool sb

instance Ord Constraint where
  sa <= sb = sbvToBool sa <= sbvToBool sb 

instance (Mergeable a, Ord a) => Mergeable (Set a) where
  symbolicMerge f t xs ys = 
    S.fromList [symbolicMerge f t x y | x <- S.toList xs, y <- S.toList ys]

instance EqSymbolic a => EqSymbolic (Set a) where
  xs .== ys = S.toList xs .== S.toList ys

instance (OrdSymbolic a, Ord a) => OrdSymbolic (Set a) where
  xs .< ys = S.toList xs .< S.toList ys

instance {-# OVERLAPPING #-} Show VarTableFunc where
  show vtf = "<<Functions of func-var-tables>>"


--
sbvToBool :: SBool -> Bool
sbvToBool sv =  
    case unliteral sv of
      Just False  -> False
      _     -> True


mrgSExpr :: SymExeExpr -> SymExeExpr -> SymExeExpr
mrgSExpr se1 se2 = S.fromList $ S.toList se1 ++ S.toList se2
  -- ite chk1 se1 $ ite chk2 se2 $ se1 S..++ se2
  -- where  
  --   chk1 = S.length se1 .>= S.length se2 .&& S.isInfixOf se2 se1
  --   chk2 = S.length se2 .>= S.length se1 .&& S.isInfixOf se1 se2


mrgConds :: [SBool] -> [SBool] -> [SBool]
mrgConds as bs = [sAnd as .|| sAnd bs] 

-------
data SymExeEnv = SEnv { procSymExeTbl :: !(IntMap VarTableFunc)  
                      , paraValMap :: !(IntMap (Value,Int))
                      , argSbvMap :: IntMap SymExeExpr
                      , blkCondMap :: IntMap Constraint
                      , controlDepGraph :: CDG
                      , domTree :: DominatorTree
                      -- , icsSumm :: IndirectCallSummary   -- pta  
                      , outSumm :: OutputSummary
                      , unsignsTbl :: UnsignedVarTbl 
                      } deriving (Generic)   


data SymExeInfo = SInfo { _instExprTable :: !(IntMap SymExeExpr)   
                        , _varValueTbl :: !VarValueTable  
                        , _blkExeCondTbl :: !(IntMap Constraint)
                       }
              deriving (Eq,Ord,Show,Generic,NFData) 

$(makeLenses ''SymExeInfo)


top :: SymExeInfo
top = SInfo mempty mempty mempty 

meetSymExeInfo :: SymExeInfo -> SymExeInfo -> SymExeInfo
meetSymExeInfo (SInfo e1 v1 c1) (SInfo e2 v2 c2) = 
          SInfo (IM.unionWith mrgSExpr e1 e2) (M.unionWith mrgSExpr v1 v2)
                (IM.unionWith (.||) c1 c2) 
{-# INLINE meetSymExeInfo #-}


--------
type GlobalSbvArgs = Set SInt32
type FormalSbvArgs = Set SInt32
type GlbFrmlArgs = IntMap (Set SInt32) -- (GlobalSbvArgs, FormalSbvArgs)
-- type SymExeSummFunc = GlbFrmlArgs -> SymExeSummary
-- type VarTableFunc = [SymExeExpr] -> VarValueTable

data SymExeSummary =
  SymExeSummary {  _varValueSummary :: VarValueTable
                , _procTblSummary :: IntMap VarTableFunc
                , _constraintTbl :: !(IntMap Constraint)
                , _paraInitSummary :: IntMap SymExeExpr
                }
  deriving (Generic,NFData,Show)

$(makeLenses ''SymExeSummary)


instance Semigroup SymExeSummary where
  (SymExeSummary vs1 ps1 ct1 pi1) <> (SymExeSummary vs2 ps2 ct2 pi2) =
    SymExeSummary (M.unionWith S.union vs1 vs2)
                  (IM.unionWith mrgVarFuncTbl ps1 ps2)
                  (IM.unionWith (.||) ct1 ct2) 
                  (IM.unionWith S.union pi1 pi2) 
   where  
     mrgVarFuncTbl tf1 tf2 = \piArgs ->
           (M.unionWith mrgSExpr) (tf1 $ IM.elems pi1) (tf2 $ IM.elems pi2)  
     piArgs = IM.elems $ IM.unionWith S.union pi1 pi2

instance Monoid SymExeSummary where
  mempty = SymExeSummary mempty mempty mempty mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif

instance Eq SymExeSummary where
  (SymExeSummary vs1 ps1 ct1 pi1) == (SymExeSummary vs2 ps2 ct2 pi2) =
        and [vs1 == vs2, ps1' == ps2', ct1 == ct2, pi1 == pi2]
   where  ps1' = IM.map (\tf -> tf (IM.elems pi1)) ps1  
          ps2' = IM.map (\tf -> tf (IM.elems pi2)) ps2
 

----
findVarWith, findIdWith :: IsValue a =>  SymExeExpr -> a -> SymExeInfo -> SymExeExpr
findVarWith sbv0 v = fromMaybe sbv0 . lkpVar (toVarName' v)
findIdWith sbv0 v  = fromMaybe sbv0 . lkpInst' (valueUniqueId v)

findVar = findVarWith nilSExpr
findId  = findIdWith nilSExpr

lkpValStr, lkpValId :: IsValue a => a -> SymExeInfo -> Maybe SymExeExpr
lkpValStr v = lkpVar (toVarName' v)
lkpValId v  = lkpInst' (valueUniqueId v)

updValStr, updValId :: IsValue a => a -> SymExeExpr -> SymExeInfo -> SymExeInfo
updValStr v = updVar (toVarName' v)
updValId v  = updInst' (valueUniqueId v)


--- For (Var,Symbolic/ConstValue) table
lkpVar :: Name -> SymExeInfo -> Maybe SymExeExpr
lkpVar x (SInfo _ vt _) =  M.lookup x vt
{-# INLINE lkpVar #-}

updVar :: Name -> SymExeExpr -> SymExeInfo -> SymExeInfo
updVar x sbv = varValueTbl %~ M.insert x sbv 

updVar2 :: [Name] -> SymExeExpr -> SymExeInfo -> SymExeInfo           
updVar2 xs sbv si = foldl' (\r x -> updVar x sbv r) si xs

updVars :: [Name] -> [SymExeExpr] -> SymExeInfo -> SymExeInfo
updVars _ [] si = si    
updVars [] _ si = si                    
updVars xs vs si = foldl' (\r (x,v) -> updVar x v r) si (zip xs vs)

xtdVar :: Name -> SymExeExpr -> SymExeInfo -> SymExeInfo
xtdVar x sbv = varValueTbl %~ M.insertWith mrgSExpr x sbv 

xtdVars :: [Name] -> [SymExeExpr] -> SymExeInfo -> SymExeInfo
xtdVars xs [] si = si            
xtdVars xs vs si = foldl' (\r (x,v) -> xtdVar x v r) si (zip xs vs)

--- For some temp (Inst,Symbolic/Value) table
lkpInst :: Instruction -> SymExeInfo -> Maybe SymExeExpr
lkpInst i =  lkpInst' (instructionUniqueId i)

lkpInst' :: ValueId -> SymExeInfo -> Maybe SymExeExpr
lkpInst' n (SInfo it _ _) =  IM.lookup n it
{-# INLINE lkpInst' #-}

updInst :: Instruction -> SymExeExpr -> SymExeInfo -> SymExeInfo
updInst i = updInst' (instructionUniqueId i)

updInst' :: ValueId -> SymExeExpr -> SymExeInfo -> SymExeInfo
updInst' n sbv  = instExprTable %~ IM.insert n sbv  

xtdInst :: ValueId -> SymExeExpr -> SymExeInfo -> SymExeInfo
xtdInst n sbv  = instExprTable %~ IM.insertWith mrgSExpr n sbv  

---  For (BasicBlock/Inst, Constrain) table
lkpCond :: IsValue a => a -> SymExeInfo -> Maybe Constraint
lkpCond i =  lkpCond' (valueUniqueId i)

lkpCond' :: ValueId -> SymExeInfo -> Maybe Constraint
lkpCond' n (SInfo _ _ ct) =  IM.lookup n ct
{-# INLINE lkpCond' #-}

updCond :: IsValue a => a -> Constraint -> SymExeInfo -> SymExeInfo
updCond i = updCond' (valueUniqueId i)

updCond' :: ValueId -> Constraint -> SymExeInfo -> SymExeInfo
updCond' n c = blkExeCondTbl %~ IM.insert n c   

updConds :: IsValue a => [a] -> [Constraint] -> SymExeInfo -> SymExeInfo
updConds _ [] si = si     
updConds [] _ si = si        
updConds is cs si = foldl' (\r (i,c) -> updCond i c r) si (zip is cs)

-------------------------
--- Some utils
unsafeCastSBV :: SBV a -> SBV b
unsafeCastSBV = SBV . unSBV

forceToIntSbv :: SInt32 -> SInt32
forceToIntSbv v = case kindOf (unSBV v) of
    KBool  ->  oneIf $ unsafeCastSBV v
    KUnbounded  -> v   -- fromSDouble sRNE $ toSDouble sRNE v
    KBounded _ _ ->  sFromIntegral $ (unsafeCastSBV v :: SInt32)
    KFloat   -> fromSFloat sRNE $ unsafeCastSBV v
    KDouble  -> fromSDouble sRNE $ unsafeCastSBV v
    KReal    -> fromSDouble sRNE $ toSDouble sRNE v
    _   ->  unsafeCastSBV v    

isFpDblValue :: SInt32 -> Bool
isFpDblValue v = case kindOf (unSBV v) of
   KFloat   ->  True
   KDouble  ->  True
   KReal    ->  True
   _        ->  False

nilSBV :: SBV a
nilSBV = unsafeCastSBV (S.nil :: SList Int32)  
nilSExpr = S.empty :: SymExeExpr

oneSExpr  = intToSExpr 1
zeroSExpr = intToSExpr 0
trueSExpr = fromSBool sTrue
falseSExpr = fromSBool sFalse
nullSExpr = toSExpr' "!Null" 

sbvToSBool :: SBV a -> SBool
sbvToSBool v = unsafeCastSBV v ./= (0 :: SInt32)
           .|| unsafeCastSBV v ./= sFalse 

toSBool, toSBool2, satSBool :: SymExeExpr -> SBool
toSBool xs = if S.null xs then sFalse 
             else sAny sbvToSBool (S.toList xs)

toSBool2 xs = sAll sbvToSBool (S.toList xs)

satSBool = fromBool . satBool

toBool, satBool :: SymExeExpr -> Bool
toBool = isSBVTrue. toSBool

satBool sbv = if S.null sbv then False
              else or. map satSbvBool $ S.toList sbv   -- and

toSList :: SymVal a => [SBV a] -> SymExeExpr
toSList = S.fromList . map unsafeCastSBV

toSExpr :: SBV a -> SymExeExpr
toSExpr = S.singleton . unsafeCastSBV 

toSExpr' :: SymVal a => a -> SymExeExpr
toSExpr' = toSExpr . literal

toSInt32 :: Integral a => a -> SInt32
toSInt32 = literal. fromIntegral

intToSExpr :: Integral a => a -> SymExeExpr
intToSExpr = S.singleton. literal. fromIntegral

fromSBool :: SBool -> SymExeExpr
fromSBool =  S.singleton . oneIf

fromDouble :: Double -> SymExeExpr
fromDouble = S.singleton . fromSDouble sRNE . literal

fromFloat :: Float -> SymExeExpr
fromFloat = S.singleton . fromSFloat sRNE . literal

anyEqSExpr, allEqSExpr :: SymExeExpr -> SymExeExpr -> SBool
anyEqSExpr se1 se2 = 
    sOr [e1 .== e2 | e1 <- S.toList se1, e2 <- S.toList se2 ]

allEqSExpr se1 se2 = se1 .== se2
    -- sAnd [e1 .== e2 | e1 <- S.toList se1, e2 <- S.toList se2 ]


---
sbvAsBool :: SBV a -> Maybe Bool
sbvAsBool = svAsBool . unSBV

isSBVTrue :: SBV a -> Bool
isSBVTrue = fromMaybe False . sbvAsBool  

satSbvBool :: SBV a -> Bool
satSbvBool v = fromMaybe b0 $ sbvAsBool v 
   where  b0 = isSat (unsafeCastSBV v :: SBool)

svToBool :: SVal -> Bool
svToBool = fromMaybe False . svAsBool

-- svNodeId :: SV -> Int
-- svNodeId (SV _ (NodeId n)) = n


------
isSat :: Provable a => a -> Bool
isSat = unsafePerformIO . isSatisfiable 


chkSatWith :: IntMap SymExeExpr -> SBool -> Bool
chkSatWith initSbvs goal = unsafePerformIO . runSMT $! do
  let sbvs0 = S.toList . S.unions $ IM.elems initSbvs
  sbvs <- mapM (const free_) sbvs0
  let constrs = [sbv .== sbv0 | (sbv, sbv0) <- zip sbvs sbvs0 ]
      chkRes = (\sbvs0 -> goal) sbvs
  sequence_ $ map constrain constrs
  constrain chkRes
  res <- query $ do
     cs <- checkSat
     case cs of 
       Unk   -> return False   -- error "Solver said unknown!"
       Unsat -> return False
       Sat   -> return True
  return res


querySat :: SMTValue a => [SymExeExpr] -> SBool -> SymExecutor [a]
querySat sxs cond = do
  lift $ constrain cond
  let xs = concatMap S.toList sxs
  lift. query $! do
     cs <- checkSat
     case cs of 
       Unk   -> return []   -- error "Solver said unknown!"
       Unsat -> return []
       Sat   -> mapM (getValue. unsafeCastSBV) xs


isOverflow :: (Integral a, Integral b) => Bool -> b -> a -> Bool
isOverflow True  sz len = (2::Integer)^(sz-1)-1 >= fromIntegral len
isOverflow False sz len = (2::Integer)^sz    -1 >= fromIntegral len

isOverflow2 x y = x == minBound && y == -1


{-
mkTuple :: (SymVal a, HasKind a) => [SBV a] -> SymExeExpr
mkTuple sbvs = case sbvs of 
      []   ->  nilSExpr
      [a]  ->  if isConcrete a 
               then toSExpr a 
               else toSExpr . SBV. SVal k. Right $ cache res
            where k      = kindOf a
                  res st = do 
                      asv <- S.sbvToSV st a
                      S.newExpr st k (S.SBVApp (S.TupleConstructor 1) [asv])
      [a,b]   -> toSExpr $! S.tuple (a,b)
      [a,b,c]   -> toSExpr $! S.tuple (a,b,c)
      [a,b,c,d]   -> toSExpr $! S.tuple (a,b,c,d)
      [a,b,c,d,e]   -> toSExpr $! S.tuple (a,b,c,d,e)
      [a,b,c,d,e,f]   -> toSExpr $! S.tuple (a,b,c,d,e,f)
      [a,b,c,d,e,f,g]   -> toSExpr $! S.tuple (a,b,c,d,e,f,g)
      [a,b,c,d,e,f,g,h]   -> toSExpr $! S.tuple (a,b,c,d,e,f,g,h)
      _     ->  toSList sbvs   
-}

---------------------------------------
--- Notes on Data.SBV
{-
newtype SBV a = SBV { unSBV :: SVal }
data SVal = SVal Kind (Either CV (Cached SV))
newtype Cached a = Cached (State -> IO a)
type Cache a   = IntMap [(StableName (State -> IO a), a)]
data SV = SV Kind NodeId
data CV = CV { _cvKind :: Kind, cvVal :: CVal }
data CVal = CAlgReal AlgReal | CInteger Integer | CFloat Float | CDouble Double | CChar Char 
           | CString String  | CList [CVal] | CUserSort (Maybe Int, String) | CTuple [CVal]
data Kind = KBool | KBounded Bool Int | KUnbounded | KUninterpreted String (Either String [String])
          | KReal | KFloat | KDouble | KChar  | KString | KList Kind | KTuple [Kind]
data Op = Plus | Times | Minus | UNeg | Abs | Quot | Rem | Equal | NotEqual | LessThan | GreaterThan
        | LessEq | GreaterEq | Ite | And | Or | XOr | Not | Shl | Shr | Rol Int | Ror Int 
        | Extract Int Int    | LkUp (Int, Kind, Kind, Int) SV SV | ArrEq  ArrayIndex ArrayIndex
        | ArrRead ArrayIndex | KindCast Kind Kind | Uninterpreted String | Label String                             -- Floating-point ops, categorized separately
        | PseudoBoolean PBOp | OverflowOp  OvOp  | StrOp StrOp  | SeqOp SeqOp 
        | TupleConstructor Int   | TupleAccess Int Int  | Join  | IEEEFP FPOp        
data OvOp = Overflow_SMul_OVFL   -- ^ Signed multiplication overflow
          | Overflow_SMul_UDFL   -- ^ Signed multiplication underflow
          | Overflow_UMul_OVFL   -- ^ Unsigned multiplication overflow
data SBVExpr = SBVApp Op [SV]           
newtype SBVPgm = SBVPgm {pgmAssignments :: S.Seq (SV, SBVExpr)}
data Assignment = Assign SVal CV


class (HasKind a, Ord a, Typeable a) => SymVal a 
   mkSymVal :: MonadSymbolic m => Maybe Quantifier -> Maybe String -> m (SBV a)
   literal :: a -> SBV a
   fromCV :: CV -> a
   unliteral :: SBV a -> Maybe a
   unliteral (SBV (SVal _ (Left c))) = Just $ fromCV c
   free :: MonadSymbolic m => String -> m (SBV a)
   free = mkSymVal Nothing . Just
   free_ :: MonadSymbolic m => m (SBV a)
   free_ = mkSymVal Nothing Nothing
   symbolic = free
   symbolics = mapM symbolic
   isConcrete :: SBV a -> Bool
   isConcrete (SBV (SVal _ (Left _))) = True

mkSymSBV :: forall a m. MonadSymbolic m => Maybe Quantifier -> Kind -> Maybe String -> m (SBV a)
mkSymSBV mbQ k mbNm = SBV <$> (symbolicEnv >>= liftIO . svMkSymVar mbQ k mbNm)
svMkSymVar :: Maybe Quantifier -> Kind -> Maybe String -> State -> IO SVal
genMkSymVar :: MonadSymbolic m => Kind -> Maybe Quantifier -> Maybe String -> m (SBV a)
genMkSymVar k mbq Nothing  = genVar_ mbq k
genMkSymVar k mbq (Just s) = genVar  mbq k s
genVar :: MonadSymbolic m => Maybe Quantifier -> Kind -> String -> m (SBV a)
genVar q k = mkSymSBV q k . Just
genVar_ :: MonadSymbolic m => Maybe Quantifier -> Kind -> m (SBV a)
genLiteral :: Integral a => Kind -> a -> SBV b
genLiteral k = SBV . SVal k . Left . mkConstCV k
fromBool :: Bool -> SBool
sBool :: MonadSymbolic m => String -> m SBool
constructUKind :: forall a. (Read a, G.Data a) => a -> Kind

svInteger :: Kind -> Integer -> SVal
svInteger k n = SVal k (Left $! mkConstCV k n)
svDouble :: Double -> SVal
svDouble d = SVal KDouble (Left $! CV KDouble (CDouble d))
svString :: String -> SVal
svString s = SVal KString (Left $! CV KString (CString s))
svAddConstant :: Integral a => SVal -> a -> SVal
svAddConstant x i = x `svPlus` svInteger (kindOf x) (fromIntegral i)
svEqual :: SVal -> SVal -> SVal
svAsBool :: SVal -> Maybe Bool
svAsInteger :: SVal -> Maybe Integer
svIte :: SVal -> SVal -> SVal -> SVal
svIte t a b = svSymbolicMerge (kindOf a) True t a b
liftViaSVal :: (SVal -> SVal -> SVal) -> SBV a -> SBV b -> SBV c
svMkOverflow :: OvOp -> SVal -> SVal -> SVal

randomCV :: Kind -> IO CV
randomCV k = CV k <$> randomCVal k
randomCVal :: Kind -> IO CVal
mkConstCV :: Integral a => Kind -> a -> CV
genFromCV :: Integral a => CV -> a
genFromCV (CV _ (CInteger x)) = fromInteger x
fromCV :: CV -> a
fromCV (CV _ (CUserSort (_, s))) = read s
cvToBool :: CV -> Bool
cvToBool x = cvVal x /= CInteger 0
toCV :: SymVal a => a -> CVal
mkCVTup :: Int -> Kind -> [CVal] -> SBV a
mkCVTup i k@(KTuple ks) = SBV $ SVal k $ Left $ CV k . CTuple 

nan :: Floating a => a
nan = 0/0
infinity :: Floating a => a
infinity = 1/0
sNaN :: (Floating a, SymVal a) => SBV a
sNaN = literal nan
sInfinity :: (Floating a, SymVal a) => SBV a
sInfinity = literal infinity
isConcreteZero :: SBV a -> Bool
isConcreteZero (SBV (SVal _     (Left (CV _     (CInteger n))))) = n == 0
isConcreteZero (SBV (SVal KReal (Left (CV KReal (CAlgReal v))))) = isExactRational v && v == 0
isConcreteZero _                                                 = False

newSV :: State -> Kind -> IO (SV, String)
newConst :: State -> CV -> IO SV
newExpr :: State -> Kind -> SBVExpr -> IO SV
reorder :: SBVExpr -> SBVExpr
newUninterpreted :: State -> String -> SBVType -> Maybe [String] -> IO ()
mkSymOpSC :: (SV -> SV -> Maybe SV) -> Op -> State -> Kind -> SV -> SV -> IO SV
mkSymOpSC shortCut op st k a b = maybe (newExpr st k (SBVApp op [a, b])) return (shortCut a b)
mkSymOp :: Op -> State -> Kind -> SV -> SV -> IO SV
mkSymOp1SC :: (SV -> Maybe SV) -> Op -> State -> Kind -> SV -> IO SV
mkSymOp1SC shortCut op st k a = maybe (newExpr st k (SBVApp op [a])) return (shortCut a)
mkSymOp1 :: Op -> State -> Kind -> SV -> IO SV
cache :: (State -> IO a) -> Cached a
cache = Cached
uncache :: Cached SV -> State -> IO SV
modifyState :: State -> (State -> IORef a) -> (a -> a) -> IO () -> IO ()
getSBVPgm :: (MonadIO m, MonadQuery m) => m SBVPgm
getSBVPgm = do State{spgm} <- queryState
               io $ readIORef spgm
getObjectives :: (MonadIO m, MonadQuery m) => m [Objective (SV, SV)]
getSBVAssertions :: (MonadIO m, MonadQuery m) => m [(String, Maybe CallStack, SV)]
getValueCV :: (MonadIO m, MonadQuery m) => Maybe Int -> SV -> m CV
svToCV = getValueCV Nothing


sbvToSymSV :: MonadSymbolic m => SBV a -> m SV
sbvToSV :: State -> SBV a -> IO SV
sbvToSV st (SBV s) = svToSV st s
svToSV :: State -> SVal -> IO SV
svToSV st (SVal _ (Left c))  = newConst st c
svToSV st (SVal _ (Right f)) = uncache f st
swToSVal :: SV -> SVal
swToSVal sv@(SV k _) = SVal k $ Right $ cache $ const $ return sv
nodeIdToSVal :: Kind -> Int -> SVal
nodeIdToSVal k i = swToSVal $ SV k (NodeId i)
sRealToSInteger :: SReal -> SInt32

type Provable = MProvable IO
class ExtractIO m => MProvable m a where
  forAll,forSome :: [String] -> a -> SymbolicT m SBool
  prove :: a -> m ThmResult
  sat :: a -> m SatResult
  allSat :: a -> m AllSatResult
  optimize :: OptimizeStyle -> a -> m OptimizeResult
  isSatisfiable :: a -> m Bool
class MonadIO m => ExtractIO m 
  extractIO :: m a -> IO (m a)
newtype ThmResult = ThmResult SMTResult
newtype SatResult = SatResult SMTResult
newtype AllSatResult = AllSatResult (Bool, Bool, [SMTResult])
newtype SafeResult   = SafeResult   (Maybe String, String, SMTResult)
proveWithAll,proveWithAny :: Provable a => [SMTConfig] -> a -> IO [(Solver, NominalDiffTime, ThmResult)]
satWithAll,satWithAny :: Provable a => [SMTConfig] -> a -> IO [(Solver, NominalDiffTime, SatResult)]
isSafe :: SafeResult -> Bool
solve :: MonadSymbolic m => [SBool] -> m SBool
solve = return . sAnd

class SolverContext m where
   -- | Add a constraint, any satisfying instance must satisfy this condition
   constrain       :: SBool -> m ()
   -- | Add a soft constraint. The solver will try to satisfy this condition if possible, but won't if it cannot
   softConstrain   :: SBool -> m ()
   -- | Add a named constraint. The name is used in unsat-core extraction.
   namedConstraint :: String -> SBool -> m ()

newtype SymbolicT m a = SymbolicT { runSymbolicT :: ReaderT State m a }
type Symbolic = SymbolicT IO
class MonadIO m => MonadSymbolic m 
  symbolicEnv :: m State
type Predicate = Symbolic SBool
type Goal = Symbolic ()
class ExtractIO m => SExecutable m a
  sName  :: [String] -> a -> SymbolicT m ()
  sName_ :: a -> SymbolicT m ()
  safe :: a -> m [SafeResult]
class Outputtable a 
  output :: MonadSymbolic m => a -> m a
output :: Outputtable a => a -> Symbolic a
outputSVal :: MonadSymbolic m => SVal -> m ()
runSymbolic :: MonadIO m => SBVRunMode -> SymbolicT m a -> m (a, Result)
runSMT :: MonadIO m => SymbolicT m a -> m a
assertWithPenalty :: String -> SBool -> Penalty -> Symbolic ()
sAssert :: HasKind a => Maybe CallStack -> String -> SBool -> SBV a -> SBV a
observe :: SymVal a => String -> SBV a -> SBV a
observeIf :: SymVal a => (a -> Bool) -> String -> SBV a -> SBV a

newtype QueryT m a = QueryT { runQueryT :: ReaderT State m a }
type Query = QueryT IO
class Monad m => MonadQuery m 
  queryState :: m State
getValue :: (MonadIO m, MonadQuery m, SMTValue a) => SBV a -> m a
getUninterpretedValue :: (MonadIO m, MonadQuery m, HasKind a) => SBV a -> m String
getSMTResult :: (MonadIO m, MonadQuery m) => m SMTResult
checkSat :: (MonadIO m, MonadQuery m) => m CheckSatResult
query :: ExtractIO m => QueryT m a -> SymbolicT m a
query = executeQuery QueryExternal
io :: IO a -> Query a
executeQuery :: forall m a. ExtractIO m => QueryContext -> QueryT m a -> SymbolicT m a
inNewContext :: (MonadIO m, MonadQuery m) => (State -> IO a) -> m a
data Assignment = Assign SVal CV
(|->) :: SymVal a => SBV a -> a -> Assignment 
mkSMTResult :: (MonadIO m, MonadQuery m) => [Assignment] -> m SMTResult
freshVar :: forall a m. (MonadIO m, MonadQuery m, SymVal a) => String -> m (SBV a)
getConfig :: (MonadIO m, MonadQuery m) => m SMTConfig
getObjectives :: (MonadIO m, MonadQuery m) => m [Objective (SV, SV)]

sFromIntegralO :: forall a b. (Integral a, HasKind a, Num a, SymVal a, HasKind b, Num b, SymVal b)
            =>   SBV a -> (SBV b, (SBool, SBool))
ranges :: forall a. (Num a, SymVal a, SMTValue a, SatModel a, Metric (SBV a))
            => (SBV a -> SBool) -> IO [Range a]

-- | The state of the symbolic interpreter
data State  = State { pathCond     :: SVal                             -- ^ kind KBool
                    , startTime    :: UTCTime
                    , runMode      :: IORef SBVRunMode
                    , rIncState    :: IORef IncState
                    , rCInfo       :: IORef [(String, CV)]
                    , rObservables :: IORef [(String, CV -> Bool, SV)]
                    , rctr         :: IORef Int
                    , rUsedKinds   :: IORef KindSet
                    , rUsedLbls    :: IORef (Set.Set String)
                    , rinps        :: IORef ([(Quantifier, NamedSymVar)], [NamedSymVar]) -- User defined, and internal existential
                    , rConstraints :: IORef [(Bool, [(String, String)], SV)]
                    , routs        :: IORef [SV]
                    , rtblMap      :: IORef TableMap
                    , spgm         :: IORef SBVPgm
                    , rconstMap    :: IORef CnstMap
                    , rexprMap     :: IORef ExprMap
                    , rArrayMap    :: IORef ArrayMap
                    , rFArrayMap   :: IORef FArrayMap
                    , rUIMap       :: IORef UIMap
                    , rCgMap       :: IORef CgMap
                    , raxioms      :: IORef [(String, [String])]
                    , rSMTOptions  :: IORef [SMTOption]
                    , rOptGoals    :: IORef [Objective (SV, SV)]
                    , rAsserts     :: IORef [(String, Maybe CallStack, SV)]
                    , rSVCache     :: IORef (Cache SV)
                    , rAICache     :: IORef (Cache ArrayIndex)
                    , rFAICache    :: IORef (Cache FArrayIndex)
                    , rQueryState  :: IORef (Maybe QueryState)
                    }
type ExprMap = Map.Map SBVExpr SV
type CnstMap = Map.Map CV SV
type TableMap = Map.Map (Kind, Kind, [SV]) Int
type ArrayInfo = (String, (Kind, Kind), ArrayContext)
type ArrayMap  = IntMap ArrayInfo
type FArrayMap  = IntMap (SVal -> SVal, IORef (IMap.IntMap SV))
type Cache a   = IntMap [(StableName (State -> IO a), a)]


-- | Result of running a symbolic computation
data Result = Result { reskinds       :: Set.Set Kind                                 -- ^ kinds used in the program
                     , resTraces      :: [(String, CV)]                               -- ^ quick-check counter-example information (if any)
                     , resObservables :: [(String, CV -> Bool, SV)]                   -- ^ observable expressions (part of the model)
                     , resUISegs      :: [(String, [String])]                         -- ^ uninterpeted code segments
                     , resInputs      :: ([(Quantifier, NamedSymVar)], [NamedSymVar]) -- ^ inputs (possibly existential) + tracker vars
                     , resConsts      :: [(SV, CV)]                                   -- ^ constants
                     , resTables      :: [((Int, Kind, Kind), [SV])]                  -- ^ tables (automatically constructed) (tableno, index-type, result-type) elts
                     , resArrays      :: [(Int, ArrayInfo)]                           -- ^ arrays (user specified)
                     , resUIConsts    :: [(String, SBVType)]                          -- ^ uninterpreted constants
                     , resAxioms      :: [(String, [String])]                         -- ^ axioms
                     , resAsgns       :: SBVPgm                                       -- ^ assignments
                     , resConstraints :: [(Bool, [(String, String)], SV)]                   -- ^ additional constraints (boolean)
                     , resAssertions  :: [(String, Maybe CallStack, SV)]              -- ^ assertions
                     , resOutputs     :: [SV]                                         -- ^ outputs
                     }
-}





