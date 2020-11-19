{-# LANGUAGE ViewPatterns, RankNTypes, DeriveGeneric, TemplateHaskell, CPP #-}
-- | This module defines the Array Analysis from the PLDI 2009 paper.
--
-- The analysis identifies which pointer parameters of a function are
-- used as arrays in that function (or any callees).  The analysis is
-- flow insensitive and works by examining chains of GetElementPtr and
-- Load instructions to reconstruct the shape of arrays that are
-- accessed.
module LLVM.Checking.Inference.Array (
  -- * Interface
  ArraySummary(..),
  identifyArrays,
  -- * Testing
  arraySummaryToTestFormat
  ) where

import GHC.Generics ( Generic )

import Control.Arrow
import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Lens', (.~), makeLenses )
import Control.Monad ( foldM )
import Data.List ( foldl' )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.Map ( Map )
import qualified Data.Map as Map
--import Data.Monoid
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

import LLVM.Analysis
import LLVM.Analysis.CallGraphSCCTraversal

import Foreign.Inference.AnalysisMonad
import Foreign.Inference.Diagnostics
import Foreign.Inference.Interface

--import Foreign.Inference.Internal.FlattenValue
import LLVM.Slicing.Util.DefRef (flattenValue)

-- import Debug.Trace
-- debug' = flip trace

-- | The real type of the summary (without the wrapper that is exposed
-- to clients).
type SummaryType = HashMap Argument Int

-- | Summarize the array parameters in the module.  This maps each
-- array argument to its inferred dimensionality.
-- newtype ArraySummary = APS SummaryType
data ArraySummary =
  ArraySummary { _arraySummary :: SummaryType
               , _arrayDiagnostics :: Diagnostics
               }
  deriving (Generic)

$(makeLenses ''ArraySummary)

instance Eq ArraySummary where
  (ArraySummary s1 _) == (ArraySummary s2 _) = s1 == s2

--instance Monoid ArraySummary where
--  mempty = ArraySummary M.empty mempty
--  mappend (ArraySummary s1 d1) (ArraySummary s2 d2) =
--    ArraySummary (M.unionWith max s1 s2) (mappend d1 d2)

instance Semigroup ArraySummary where
  (ArraySummary s1 d1) <> (ArraySummary s2 d2) =
    ArraySummary (M.unionWith max s1 s2) (d1 <> d2)

instance Monoid ArraySummary where
  mempty = ArraySummary M.empty mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif

instance NFData ArraySummary where
  rnf = genericRnf

instance HasDiagnostics ArraySummary where
  diagnosticLens = arrayDiagnostics

instance SummarizeModule ArraySummary where
  summarizeFunction _ _ = []
  summarizeArgument = summarizeArrayArgument

summarizeArrayArgument :: Argument -> ArraySummary -> [(ParamAnnotation, [Witness])]
summarizeArrayArgument a (ArraySummary summ _) =
  case M.lookup a summ of
    Nothing -> []
    Just depth -> [(PAArray depth, [])]

type Analysis = AnalysisMonad () ()

-- | A data type to capture uses of pointers in array contexts.  These
-- are accumulated in one pass over the function and then used to
-- reconstruct the shapes of arrays in the tracing functions.
data PointerUse = IndexOperation Value [Value]
                | CallArgument Int
                deriving (Show)

identifyArrays :: (FuncLike funcLike, HasFunction funcLike)
                  => DependencySummary
                  -> Lens' compositeSummary ArraySummary
                  -> ComposableAnalysis compositeSummary funcLike
identifyArrays ds =
  composableAnalysisM runner arrayAnalysis
  where
    runner a = runAnalysis a ds () ()

-- | The summarization function - add a summary for the current
-- Function to the current summary.  This function collects all of the
-- base+offset pairs and then uses @traceFromBases@ to reconstruct
-- them.
arrayAnalysis :: (FuncLike funcLike, HasFunction funcLike)
                 => funcLike -> ArraySummary -> Analysis ArraySummary
arrayAnalysis funcLike a@(ArraySummary summary _) = do
  basesAndOffsets <- mapM (isArrayDeref a) insts
--  let basesAndOffsets = map (isArrayDeref ds a) insts
  let baseResultMap = foldr (\itm acc -> foldr addDeref acc itm) M.empty basesAndOffsets
      summary' = M.foldlWithKey' (traceFromBases baseResultMap) summary baseResultMap
  return $! (arraySummary .~ summary') a
  where
    f = getFunction funcLike
    insts = concatMap basicBlockInstructions (functionBody f)
    addDeref (base, use) = M.insertWith (++) base [use]

-- | Examine a GetElementPtr instruction result.  If the base is an
-- argument, trace its access structure (using the @baseResultMap@ via
-- @traceBackwards@) and record the dimensions in the summary.
--
-- Otherwise, just pass the summary along and try to find the next
-- access.
traceFromBases :: HashMap Value [PointerUse]
                  -> SummaryType
                  -> Value
                  -> [PointerUse]
                  -> SummaryType
traceFromBases baseResultMap summary base uses =
  -- FIXME: This test of argumentness needs to be extended to take
  -- into account PHI nodes (also selects)
  case valueContent' base of
    ArgumentC a ->
      let depth = maximum $ map dispatchTrace uses
      in addToSummary depth a summary
    _ -> summary
  where
    dispatchTrace (IndexOperation result _) =
      traceBackwards baseResultMap result 1
    dispatchTrace (CallArgument depth) = depth

-- | Update the summary for an argument with a depth.
--
-- The function always keeps the *maximum* array depth it discovers
-- (i.e., inserting an array depth of 1 for an argument that is
-- already recorded as having depth 2 will not make any changes to the
-- summary).
addToSummary :: Int -> Argument -> SummaryType -> SummaryType
addToSummary depth arg =
  M.insertWith max arg depth


traceBackwards :: HashMap Value [PointerUse] -> Value -> Int -> Int
traceBackwards baseResultMap result depth =
  -- Is the current result used as the base of an indexing operation?
  -- If so, that adds a level of array wrapping.
  case M.lookup result baseResultMap of
    Nothing -> depth
    Just uses -> maximum (map dispatchTrace uses)
  where
    dispatchTrace use =
      case use of
        IndexOperation result' _ -> traceBackwards baseResultMap result' (depth + 1)
        CallArgument d -> depth + d

isArrayDeref :: ArraySummary
                -> Instruction
                -> Analysis [(Value, PointerUse)]
isArrayDeref summ inst =
  case valueContent' inst of
    InstructionC LoadInst { loadAddress = (valueContent ->
       InstructionC GetElementPtrInst { getElementPtrValue = base
                                     , getElementPtrIndices = idxs
                                     })} ->
      return $ handleGEP idxs base
    InstructionC StoreInst { storeAddress = (valueContent ->
       InstructionC GetElementPtrInst { getElementPtrValue = base
                                      , getElementPtrIndices = idxs
                                      })} ->
      return $ handleGEP idxs base
    InstructionC AtomicCmpXchgInst { atomicCmpXchgPointer = (valueContent ->
      InstructionC GetElementPtrInst { getElementPtrValue = base
                                      , getElementPtrIndices = idxs
                                      })} ->
      return $ handleGEP idxs base
    InstructionC AtomicRMWInst { atomicRMWPointer = (valueContent ->
      InstructionC GetElementPtrInst { getElementPtrValue = base
                                      , getElementPtrIndices = idxs
                                      })} ->
      return $ handleGEP idxs base
    InstructionC CallInst { callFunction = f, callArguments = args } ->
      let indexedArgs = zip [0..] (map fst args)
      in foldM (collectArrayArgs summ f) [] (concatMap expand indexedArgs)
    InstructionC InvokeInst { invokeFunction = f, invokeArguments = args } ->
      let indexedArgs = zip [0..] (map fst args)
      in foldM (collectArrayArgs summ f) [] (concatMap expand indexedArgs)
    _ -> return []
  where
    expand (ix, a) = zip (repeat ix) (flattenValue a)
    handleGEP idxs base =
      let flatVals = flattenValue base
      in foldl' (buildArrayDeref inst idxs) [] flatVals

buildArrayDeref :: Instruction -> [Value] -> [(Value, PointerUse)] -> Value -> [(Value, PointerUse)]
buildArrayDeref inst idxs acc base =
  case idxs of
    [] -> error ("LLVM.Checking.buildArrayDeref: GEP with no indices: " ++ show inst)
    [_] -> (base, IndexOperation (toValue inst) idxs) : acc
    (valueContent' -> ConstantC ConstantInt { constantIntValue = 0 }) : _ -> acc
    _ -> (base, IndexOperation (toValue inst) idxs ) : acc

-- | If the argument is an array (according to either the module
-- summary or the dependency summary), make a CallArgument tag for it.
-- Only bother inspecting direct calls.  Information gained from
-- indirect calls is unreliable since we don't have all possible
-- callees, even with a very powerful points-to analysis.
collectArrayArgs :: ArraySummary
                    -> Value
                    -> [(Value, PointerUse)]
                    -> (Int, Value)
                    -> Analysis [(Value, PointerUse)]
collectArrayArgs summ callee lst (ix, arg) = do
  annots <- lookupArgumentSummaryList summ callee ix
  case filter isArrayAnnot annots of
    [] -> return lst
    [PAArray depth] -> return $ (arg, CallArgument depth) : lst
    _ -> do
      emitError Nothing "Array.collectArrayArgs" "Summary should only produce singleton or empty lists"
      return lst


isArrayAnnot :: ParamAnnotation -> Bool
isArrayAnnot (PAArray _) = True
isArrayAnnot _ = False


-- Testing

arraySummaryToTestFormat :: ArraySummary -> Map (String, String) Int
arraySummaryToTestFormat (ArraySummary summ _) =
  Map.fromList $ map (first argToString) $ M.toList summ
  where
    argToString a =
      let f = argumentFunction a
      in (show (functionName f), show (argumentName a))
