{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE DeriveGeneric, ViewPatterns, TemplateHaskell, CPP #-}
module Foreign.Inference.Analysis.Escape (
  EscapeSummary,
  identifyEscapes,
  instructionEscapes,
  instructionEscapesWith,

  -- * Testing
  EscapeClass(..),
  escapeResultToTestFormat,
  -- escapeUseGraphs,
  -- useGraphvizRepr
  ) where

import GHC.Generics ( Generic )

import Control.DeepSeq
import Control.DeepSeq.Generics ( genericRnf )
import Control.Lens ( Lens', (^.), (%~), makeLenses )
import Control.Monad ( foldM )
import qualified Data.Foldable as F
import Data.Hashable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.List ( mapAccumR )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, isNothing, mapMaybe )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Monoid (First(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Safe.Failure ( at )
import Text.Printf

import LLVM.Analysis
import LLVM.Analysis.AccessPath
import LLVM.Analysis.CallGraphSCCTraversal

import Constraints.Set.Solver

import Foreign.Inference.Diagnostics ( HasDiagnostics(..), Diagnostics )
import Foreign.Inference.Interface
import Foreign.Inference.Internal.FlattenValue
import Foreign.Inference.AnalysisMonad
import Foreign.Inference.Analysis.IndirectCallResolver

-- import System.IO.Unsafe
-- import Text.Printf
-- import Debug.Trace
-- debug = flip trace

-- | The ways a value can escape from a function
data EscapeClass = DirectEscape
                 | BrokenContractEscape
                 | IndirectEscape
                 | ArgumentEscape !Int -- ^ Index escaped into
                 deriving (Eq, Ord, Read, Show, Generic)

instance Hashable EscapeClass where
  hashWithSalt s DirectEscape = s `hashWithSalt` (76 :: Int)
  hashWithSalt s BrokenContractEscape = s `hashWithSalt` (699 :: Int)
  hashWithSalt s IndirectEscape = s `hashWithSalt` (5 :: Int)
  hashWithSalt s (ArgumentEscape i) =
    s `hashWithSalt` (77997 :: Int) `hashWithSalt` i

instance NFData EscapeClass

data ArgumentDescriptor = ArgumentDescriptor Function Int
                        deriving (Eq, Ord, Show, Generic)

instance NFData ArgumentDescriptor where
  rnf = genericRnf

data Constructor = Sink { sinkClass :: EscapeClass
                        , sinkWitness :: Instruction
                        , sinkIntoArgument :: Maybe ArgumentDescriptor
                        }
                 deriving (Eq, Ord, Show, Generic)

data Var = Location !Value
         | FieldSource { fieldSourceArgument :: !Argument
                       , fieldSourcePath :: AbstractAccessPath
                       }
         deriving (Eq, Ord, Show, Generic)

type SetExp = SetExpression Var Constructor
type ValueFlowGraph = SolvedSystem Var Constructor

data EscapeGraph = EscapeGraph {
  escapeGraphFieldSourceMap :: HashMap Argument [AbstractAccessPath],
  escapeVFG :: ValueFlowGraph
  } deriving (Eq, Generic)

-- zyz
instance Generic (SolvedSystem a b)
instance NFData (SolvedSystem a b) where
  rnf _ = ()

instance NFData EscapeGraph

-- | The monad in which we construct the value flow graph
-- type GraphBuilder = State GraphState

data EscapeSummary =
  EscapeSummary { _escapeGraph :: HashMap Function EscapeGraph
                , _escapeArguments :: HashMap Argument (EscapeClass, Instruction)
                , _escapeFields :: HashMap Argument (Set (EscapeClass, AbstractAccessPath, Instruction))
                , _escapeIntoArguments :: HashMap Argument (EscapeClass, Function, Int)
                , _escapeDiagnostics :: Diagnostics
                }
  deriving (Generic)

$(makeLenses ''EscapeSummary)

instance Show EscapeSummary where
  show (EscapeSummary _ ea ef ei _) = show ea ++ "/" ++ show ef ++ "/" ++ show ei

instance Eq EscapeSummary where
  (EscapeSummary g1 ea1 ef1 ei1 _) == (EscapeSummary g2 ea2 ef2 ei2 _) =
    g1 == g2 && ea1 == ea2 && ef1 == ef2 && ei1 == ei2

emptySummary :: EscapeSummary
emptySummary = EscapeSummary mempty mempty mempty mempty mempty

instance Semigroup EscapeSummary where
  (EscapeSummary g1 as1 was1 ei1 d1) <> (EscapeSummary g2 as2 was2 ei2 d2) =
    EscapeSummary { _escapeGraph = HM.union g1 g2
                  , _escapeArguments = HM.union as1 as2
                  , _escapeFields = HM.union was1 was2
                  , _escapeIntoArguments = HM.union ei1 ei2
                  , _escapeDiagnostics = d1 <> d2
                  }


instance Monoid EscapeSummary where
  mempty = emptySummary
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif

instance NFData EscapeSummary where
  rnf = genericRnf

instance HasDiagnostics EscapeSummary where
  diagnosticLens = escapeDiagnostics

instance SummarizeModule EscapeSummary where
  summarizeFunction _ _ = []
  summarizeArgument = summarizeEscapeArgument

type Analysis = AnalysisMonad () ()

-- | This is the underlying bottom-up analysis to identify which
-- arguments escape.  It builds an EscapeGraph for the function
-- (incorporating information from other functions that have already
-- been analyzed) and then checks to see which arguments escape using
-- that graph.
identifyEscapes :: (FuncLike funcLike, HasFunction funcLike)
                   => DependencySummary
                   -> IndirectCallSummary
                   -> Lens' compositeSummary EscapeSummary
                   -> ComposableAnalysis compositeSummary funcLike
identifyEscapes ds ics lns =
  composableAnalysisM runner escapeWrapper lns
  where
    runner a = runAnalysis a ds () ()
    escapeWrapper funcLike s = do
      let f = getFunction funcLike
      g <- buildValueFlowGraph ics s (functionInstructions f)
      let s' = foldr (summarizeArgumentEscapes g) s (functionParameters f)
      return $ (escapeGraph %~ HM.insert f g) s'

{-
    extSumm ef ix =
      -- FIXME: Switch the builder to be a StateT so we can let this
      -- monadic extsumm record missing summaries
      case lookupArgumentSummary ds (undefined :: EscapeSummary) ef ix of
        Nothing -> True --  do
          -- let msg = "Missing summary for " ++ show (externalFunctionName ef)
          -- emitWarning Nothing "EscapeAnalysis" msg
          -- return True
        Just annots -> PAEscape `elem` annots
-}

-- | A generalization of 'instructionEscapes'.  The first argument is
-- a predicate that returns True if the input Instruction (which is a
-- sink) should be excluded from the reachability search of the value
-- flow graph.
--
-- The intended use of this variant is to issue escape queries for
-- instructions that are known to escape via some desired means (e.g.,
-- an out parameter) and to determine if they also escape via some
-- other means.  In that case, the @ignore@ predicate should return
-- True for the store instruction that created the known escape.
instructionEscapesWith :: (Instruction -> Bool)
                          -> Instruction
                          -> EscapeSummary
                          -> Maybe Instruction
instructionEscapesWith = instructionEscapeCore

-- | Returns the instruction (if any) that causes the input
-- instruction to escape.  This does *not* cover WillEscape at all.
instructionEscapes :: Instruction -> EscapeSummary -> Maybe Instruction
instructionEscapes = instructionEscapeCore (const False)

-- | This is shared code for all of the instruction escape queries.
--
-- Most of the description is on 'instructionEscapesWith'
instructionEscapeCore :: (Instruction -> Bool)
                         -> Instruction
                         -> EscapeSummary
                         -> Maybe Instruction
instructionEscapeCore ignorePred i (EscapeSummary egs _ _ _ _) = do
  f <- instructionFunction i
  EscapeGraph _ eg <- HM.lookup f egs
  ts@(_:_) <- leastSolution eg (Location (toValue i))
  let sinks = map toSink ts
      sinks' = filter (not . ignorePred . sinkWitness) sinks
  case sinks' of
    [] -> Nothing
    s:_ -> return (sinkWitness s)

summarizeEscapeArgument :: Argument -> EscapeSummary -> [(ParamAnnotation, [Witness])]
summarizeEscapeArgument a er
  | not (isPointerType a) = []
  | otherwise =
    case HM.lookup a (er ^. escapeArguments) of
      Nothing -> []
      Just (DirectEscape, w@RetInst {}) -> [(PAWillEscape, [Witness w "ret"])]
      Just (t, w@StoreInst {}) -> [(tagToAnnot t, [Witness w "store"])]
      Just (t, w@CallInst {}) -> [(tagToAnnot t, [Witness w "call"])]
      Just (t, w@InvokeInst {}) -> [(tagToAnnot t, [Witness w "call"])]
      Just (t, w) -> [(tagToAnnot t, [Witness w "access"])]
  where
    tagToAnnot t =
      case t of
        DirectEscape -> PAEscape
        IndirectEscape -> PAFptrEscape
        BrokenContractEscape -> PAContractEscape
        ArgumentEscape ix -> PAArgEscape ix

takeFirst :: a -> [Maybe a] -> a
takeFirst def [] = def
takeFirst def (act:rest) =
  case act of
    Nothing -> takeFirst def rest
    Just thing -> thing

summarizeArgumentEscapes :: EscapeGraph -> Argument -> EscapeSummary -> EscapeSummary
summarizeArgumentEscapes eg a s =
  takeFirst s [ entireArgumentEscapes eg a s
              , argumentFieldsEscape eg a s
              ]

toSink :: SetExp -> Constructor
toSink (ConstructedTerm e _ []) = e
toSink e = error ("Foreign.Inference.Analysis.Escape.toSink: Unexpected non-constructed term: " ++ show e)

entireArgumentEscapes :: EscapeGraph -> Argument -> EscapeSummary -> Maybe EscapeSummary
entireArgumentEscapes (EscapeGraph _ eg) a s = do
  ts@(_:_) <- leastSolution eg (Location (toValue a))
  let sink:_ = map toSink ts
  return $ (escapeArguments %~ HM.insert a (sinkClass sink, sinkWitness sink)) s

argumentFieldsEscape :: EscapeGraph -> Argument -> EscapeSummary -> Maybe EscapeSummary
argumentFieldsEscape (EscapeGraph fields eg) a s = do
  fieldPaths <- HM.lookup a fields
  return $ foldr fieldEscapes s fieldPaths
  where
    fieldEscapes fldPath acc = fromMaybe acc $ do
      ts@(_:_) <- leastSolution eg (FieldSource a fldPath)
      let sink:_ = map toSink ts
          entry = S.singleton (sinkClass sink, fldPath, sinkWitness sink)
      return $ (escapeFields %~ HM.insertWith S.union a entry) acc

notPointer :: IsValue v => v -> Bool
notPointer v =
  case valueType v of
    TypePointer _ _ -> False
    _ -> True

buildValueFlowGraph :: IndirectCallSummary
                       -> EscapeSummary
                       -> [Instruction]
                       -> Analysis EscapeGraph
buildValueFlowGraph ics summ is = do
  (inclusionSystem, fieldSrcs) <- foldM addInclusion ([], mempty) is
  let Just sys = solveSystem inclusionSystem
  return $ EscapeGraph { escapeGraphFieldSourceMap = fieldSrcs
                       , escapeVFG = sys
                       }
  where
    sinkExp klass witness argDesc = atom (Sink klass witness argDesc)
    setExpFor v =
      case valueContent' v of
        InstructionC i@GetElementPtrInst { } ->
          case argumentBasedField i of
            Nothing -> setVariable (Location (stripBitcasts v))
            Just (a, aap) -> setVariable (FieldSource a aap)
        InstructionC i@LoadInst { } ->
          case argumentBasedField i of
            Nothing -> setVariable (Location (stripBitcasts v))
            Just (a, aap) -> setVariable (FieldSource a aap)
        _ -> setVariable (Location (stripBitcasts v))

    addInclusion :: ([Inclusion Var Constructor], HashMap Argument [AbstractAccessPath])
                    -> Instruction
                    -> Analysis ([Inclusion Var Constructor], HashMap Argument [AbstractAccessPath])
    addInclusion acc@(incs, fsrcs) i =
      case i of
        RetInst { retInstValue = Just (valueContent' -> rv) } ->
          let s = sinkExp DirectEscape i Nothing
              c = s <=! setExpFor rv
          in return (c : incs, fsrcs)
        -- If this is a load of an argument field, we need to make it
        -- into a FieldSource and see what happens to it later.
        -- Record the argument/access path in a map somewhere for
        -- later lookup (otherwise we can't find the variable)
        GetElementPtrInst {} ->
          case argumentBasedField i of
            Just (a, aap) ->
              let c = setExpFor (toValue i) <=! setVariable (FieldSource a aap)
                  srcs' = HM.insertWith (++) a [aap] fsrcs
              in return (c : incs, srcs')
            Nothing -> return acc
        LoadInst { loadAddress = la }
          | notPointer i || isNothing (argumentBasedField i) -> return acc
          | otherwise ->
            let c = setExpFor (toValue i) <=! setExpFor la
            in return (c : incs, fsrcs)
        StoreInst { storeAddress = sa
                  , storeValue = sv
                  }
          | mustEsc ->
            let sinkTag = maybe DirectEscape (ArgumentEscape . argumentIndex) mArg
                s = sinkExp sinkTag i Nothing
                c = s <=! setExpFor sv
            in return (c : incs, fsrcs)
          | otherwise ->
              -- May escape later if the alloca escapes
              let c = setExpFor sa <=! setExpFor sv
              in return (c : incs, fsrcs)
          where
            (mustEsc, mArg) = mustEscapeLocation sa

        CallInst { callFunction = callee, callArguments = (map (stripBitcasts . fst) -> args) } ->
          addCallConstraints i acc callee args
        InvokeInst { invokeFunction = callee, invokeArguments = (map (stripBitcasts . fst) -> args) } ->
          addCallConstraints i acc callee args
        SelectInst { selectTrueValue = (valueContent' -> tv)
                   , selectFalseValue = (valueContent' -> fv)
                   } ->
          let c1 = setExpFor (toValue i) <=! setExpFor tv
              c2 = setExpFor (toValue i) <=! setExpFor fv
          in return (c1 : c2 : incs, fsrcs)
        PhiNode { phiIncomingValues = (map (stripBitcasts . fst) -> ivs) } ->
          let toIncl v = setExpFor (toValue i) <=! setExpFor v
              cs = map toIncl ivs
          in return (cs ++ incs, fsrcs)
        _ -> return acc

    addCallConstraints :: Instruction
                          -> ([Inclusion Var Constructor], HashMap Argument [AbstractAccessPath])
                          -> Value
                          -> [Value]
                          -> Analysis ([Inclusion Var Constructor], HashMap Argument [AbstractAccessPath])
    addCallConstraints callInst (incs, fsrcs) callee args =
      case valueContent' callee of
        FunctionC f -> do
          let indexedArgs = zip [0..] args
          incs' <- foldM (addActualConstraint callInst f) incs indexedArgs
          return (incs', fsrcs)
        ExternalFunctionC ef -> do
          let indexedArgs = zip [0..] args
          incs' <- foldM (addActualConstraint callInst ef) incs indexedArgs
          return (incs', fsrcs)
        _ ->
          case indirectCallInitializers ics callee of
            -- No targets known; all pointer arguments indirectly escape
            [] -> do
              incs' <- foldM (addIndirectEscape callInst) incs args
              return (incs', fsrcs)
            -- We have at least one target; take it as a representative
            (repr:_) -> do
              let indexedArgs = zip [0..] args
              incs' <- foldM (addContractEscapes callInst repr) incs indexedArgs
              return (incs', fsrcs)

    argEscapeConstraint callInst etype actual incs =
      -- FIXME; figure out how to use the index in a field escape here
      let s = sinkExp etype callInst Nothing
          c = s <=! setExpFor actual
      in return $ c : incs

    addContractEscapes :: Instruction
                          -> Value
                          -> [Inclusion Var Constructor]
                          -> (Int, Value)
                          -> Analysis [Inclusion Var Constructor]
    addContractEscapes callInst repr incs (ix, actual)
      | notPointer actual = return incs
      | otherwise = do
        s <- lookupArgumentSummary summ repr ix

        case s of
          -- If we don't have a summary for our representative, treat
          -- it as an indirect call with no known target (we could do
          -- better by looking at the next possible representative, if
          -- any).
          Nothing -> addIndirectEscape callInst incs actual
          Just pannots ->
            case F.find isEscapeAnnot pannots of
              -- If we don't find an escape annotation, we generate a
              -- BrokenContractEscape since the argument will only
              -- escape if the function pointer breaks a contract
              Nothing -> argEscapeConstraint callInst BrokenContractEscape actual incs
              Just PAEscape -> argEscapeConstraint callInst DirectEscape actual incs
              Just PAContractEscape -> argEscapeConstraint callInst BrokenContractEscape actual incs
              Just PAFptrEscape -> argEscapeConstraint callInst IndirectEscape actual incs
              _ -> return incs

    addActualConstraint callInst callee incs (ix, actual) = do
      pannots <- lookupArgumentSummaryList summ callee ix
      case F.find isEscapeAnnot pannots of
        Nothing -> return incs
        Just PAEscape ->  argEscapeConstraint callInst DirectEscape actual incs
        Just PAContractEscape -> argEscapeConstraint callInst BrokenContractEscape actual incs
        Just PAFptrEscape -> argEscapeConstraint callInst IndirectEscape actual incs
        Just (PAArgEscape argIx)
          | callInstActualIsAlloca callInst argIx -> return incs
          | otherwise ->
            argEscapeConstraint callInst (ArgumentEscape argIx) actual incs
        _ -> return incs

-- Note, it isn't quite obvious what to do with PAArgEscape here.

    addIndirectEscape callInst incs actual
      | notPointer actual = return incs
      | otherwise = argEscapeConstraint callInst IndirectEscape actual incs

-- FIXME This should be a "not address taken" alloca - that is, not
-- passed to any functions.
callInstActualIsAlloca :: Instruction -> Int -> Bool
callInstActualIsAlloca i ix =
  case i of
    CallInst { callArguments = (map fst -> args) } ->
      isAlloca args
    InvokeInst { invokeArguments = (map fst -> args) } ->
      isAlloca args
    _ -> False
  where
    isAlloca args =
      fromMaybe False $ do
        actual <- args `at` ix
        actualInst <- fromValue actual
        case actualInst of
          AllocaInst {} -> return True
          _ -> fail "Not an alloca"

isEscapeAnnot :: ParamAnnotation -> Bool
isEscapeAnnot a =
  case a of
    PAEscape -> True
    PAArgEscape _ -> True
    PAContractEscape -> True
    PAFptrEscape -> True
    _ -> False
-- Ignore PAWillEscape for now...

isPointerType :: (IsValue a) => a -> Bool
isPointerType v =
  case valueType v of
    TypePointer _ _ -> True
    _ -> False

-- Given a GetElementPtrInst, return its base and the path accessed
-- IFF the base was an Argument.
argumentBasedField :: Instruction -> Maybe (Argument, AbstractAccessPath)
argumentBasedField li = do
  accPath <- accessPath li
  case valueContent' (accessPathBaseValue accPath) of
    ArgumentC a -> return (a, abstractAccessPath accPath)
    _ -> Nothing

mustEscapeLocation :: Value -> (Bool, Maybe Argument)
mustEscapeLocation = snd . go mempty
  where
    go visited v
      | S.member v visited = (visited, (False, Nothing))
      | otherwise =
        case valueContent' v of
          GlobalVariableC _ -> (visited', (True, Nothing))
          ExternalValueC _ -> (visited', (True, Nothing))
          ArgumentC a -> (visited', (True, Just a))
          InstructionC CallInst {} -> (visited', (True, Nothing))
          InstructionC InvokeInst {} -> (visited', (True, Nothing))
          InstructionC LoadInst { loadAddress = la } ->
            go visited' la
          InstructionC GetElementPtrInst { getElementPtrValue = base } ->
            go visited' base
          InstructionC SelectInst { } ->
            let (visited'', pairs) = mapAccumR go visited' (flattenValue v)
                argVal = mconcat $ map (First . snd) pairs
            in (visited'', (any fst pairs, getFirst argVal))
          InstructionC PhiNode {} ->
            let (visited'', pairs) = mapAccumR go visited' (flattenValue v)
                argVal = mconcat $ map (First . snd) pairs
            in (visited'', (any fst pairs, getFirst argVal))
          _ -> (visited', (False, Nothing))
      where
        visited' = S.insert v visited

-- Testing

-- | Extract the arguments for each function that escape.  The keys of
-- the map are function names and the set elements are argument names.
-- This format exposes the internal results for testing purposes.
--
-- For actual use in a program, use one of 'functionEscapeArguments',
-- 'functionWillEscapeArguments', or 'instructionEscapes' instead.
escapeResultToTestFormat :: EscapeSummary -> Map String (Set (EscapeClass, String))
escapeResultToTestFormat er =
  M.filter (not . S.null) $ foldr fieldTransform argEscapes (HM.toList fm)
  where
    directEscapes = foldr transform mempty (HM.toList m)
    argEscapes = foldr argTransform directEscapes (HM.toList am)
    m = er ^. escapeArguments
    fm = er ^. escapeFields
    am = er ^. escapeIntoArguments
    argTransform (a, (tag, _, _)) acc =
      let aname = show (argumentName a)
          f = argumentFunction a
          fname = show (functionName f)
      in M.insertWith' S.union fname (S.singleton (tag, aname)) acc
    transform (a, (tag, _)) acc =
      let f = argumentFunction a
          fname = show (functionName f)
          aname = show (argumentName a)
      in M.insertWith' S.union fname (S.singleton (tag, aname)) acc
    fieldTransform (a, fieldsAndInsts) acc =
      let f = argumentFunction a
          fname = show (functionName f)
          aname = show (argumentName a)
          tagsAndFields = S.toList $ S.map (\(tag, fld, _) -> (tag, fld)) fieldsAndInsts
          newEntries = S.fromList $ mapMaybe (toFieldRef aname) tagsAndFields
      in M.insertWith' S.union fname newEntries acc
    toFieldRef aname (tag, fld) =
      case abstractAccessPathComponents fld of
        [AccessField ix] -> Just $ (tag, printf "%s.<%d>" aname ix)
        _ -> Nothing
