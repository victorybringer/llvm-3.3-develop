{-# LANGUAGE RankNTypes, TemplateHaskell, DeriveGeneric, CPP #-}
{-| This module defines a data type that can be used as the summary
type for a composite analysis using all of the analyses defined in
this package.

It is useful to have it defined in a common module so it can be
re-used for all of the tests and the driver program.

Additionally, moving it to the library (instead of duplicating it in
each executable) makes it easier to use TemplateHaskell here to
generate lenses.

-}
-- Changing from Foreign.Inference.Analysis.Util.CompositeSummary
module LLVM.Checking.CheckSummary (
  FunctionMetadata(..),
  AnalysisSummary(..),
  nullableSummary,
  outputSummary,
  arraySummary,
  returnSummary,
  finalizerSummary,
  escapeSummary,
  allocatorSummary,
  refCountSummary,
  sapSummary,
  sapPTRelSummary,
  scalarEffectSummary,
  errorHandlingSummary,
  transferSummary,
  extractSummary,
  -- for slicing
  symSliceSummary,
  infoSliceSummary,
  -- for checking
  defectSummary,
  -- for executing
  symExeSummary
  
  ) where

import GHC.Generics

import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Lens
--import Data.Monoid
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

import LLVM.Analysis
import LLVM.Analysis.BlockReturnValue
import LLVM.Analysis.Dominance
import LLVM.Analysis.CDG
import LLVM.Analysis.CFG
import LLVM.Analysis.NullPointers

import qualified LLVM.Checking.Inference.Allocator as Al
import qualified LLVM.Checking.Inference.Array  as Ar
import qualified LLVM.Checking.Inference.ErrorHandling as EH
import qualified LLVM.Checking.Inference.Escape as Es
import qualified LLVM.Checking.Inference.Finalize as Fi
import qualified LLVM.Checking.Inference.Nullable as Nu
import qualified LLVM.Checking.Inference.Output as Ou
import qualified LLVM.Checking.Inference.RefCount as RC
import qualified LLVM.Checking.Inference.Return as Re
import qualified LLVM.Checking.Inference.SAP as Sap
import qualified LLVM.Checking.Inference.SAPPTRel as SP
import qualified LLVM.Checking.Inference.ScalarEffects as SE
import qualified LLVM.Checking.Inference.Transfer as Tr

import qualified LLVM.Checking.Defect as De
import qualified LLVM.Executing.SymExeType as SymE

import Foreign.Inference.Diagnostics
import Foreign.Inference.Interface

--
import qualified LLVM.Slicing.Static.Symbolic.SymSlicer0 as Sym
import qualified LLVM.Slicing.Static.InfoFlow.InfoFlowSlicer as IF 


-- | The value we derive from each function during the call graph
-- traversal.  For now, it just adds a CFG.
data FunctionMetadata =
  FunctionMetadata { functionOriginal :: Function
                   , functionCFG :: CFG
                   , functionCDG :: CDG
                   , functionDomTree :: DominatorTree
                   , functionPostdomTree :: PostdominatorTree
                   , functionBlockReturns :: BlockReturns
                   , functionNullPointers :: NullPointersSummary
                   }

instance HasNullSummary FunctionMetadata where
  getNullSummary = functionNullPointers

instance HasBlockReturns FunctionMetadata where
  getBlockReturns = functionBlockReturns

instance HasFunction FunctionMetadata where
  getFunction = functionOriginal

instance HasCFG FunctionMetadata where
  getCFG = functionCFG

instance HasDomTree FunctionMetadata where
  getDomTree = functionDomTree

instance HasPostdomTree FunctionMetadata where
  getPostdomTree = functionPostdomTree

instance FuncLike FunctionMetadata where
  fromFunction f =
    FunctionMetadata { functionOriginal = f
                     , functionCFG = cfg
                     , functionCDG = controlDependenceGraph cfg
                     , functionDomTree = dominatorTree cfg
                     , functionPostdomTree = postdominatorTree cfg
                     , functionBlockReturns = labelBlockReturns cfg
                     , functionNullPointers = nullPointersAnalysis cfg
                     }
    where
      cfg = controlFlowGraph f
--      cdg = controlDependenceGraph cfg

instance HasCDG FunctionMetadata where
  getCDG = functionCDG

-- | A type containing all of the sub-summaries.
data AnalysisSummary =
  AnalysisSummary { _nullableSummary :: !Nu.NullableSummary
                  , _outputSummary :: !Ou.OutputSummary
                  , _arraySummary :: !Ar.ArraySummary
                  , _returnSummary :: !Re.ReturnSummary
                  , _finalizerSummary :: !Fi.FinalizerSummary
                  , _escapeSummary :: !Es.EscapeSummary
                  , _allocatorSummary :: !Al.AllocatorSummary
                  , _refCountSummary :: !RC.RefCountSummary
                  , _scalarEffectSummary :: !SE.ScalarEffectSummary
                  , _errorHandlingSummary :: !EH.ErrorSummary
                  , _transferSummary :: !Tr.TransferSummary
                  , _sapSummary :: !Sap.SAPSummary
                  , _sapPTRelSummary :: !SP.SAPPTRelSummary
     -- based on slicing
                  , _symSliceSummary :: !Sym.SliceSummary
                  , _infoSliceSummary :: !IF.SliceSummary
              -- checking
                  , _defectSummary :: De.DefectSummary
              -- executing
                  , _symExeSummary :: SymE.SymExeSummary
                  }
  deriving (Eq, Generic)

$(makeLenses ''AnalysisSummary)

instance NFData AnalysisSummary where
  rnf = genericRnf
  
instance Semigroup AnalysisSummary where
  a1 <> a2 =
    AnalysisSummary { _nullableSummary = _nullableSummary a1 <> _nullableSummary a2
                    , _outputSummary = _outputSummary a1 <> _outputSummary a2
                    , _arraySummary = _arraySummary a1 <> _arraySummary a2
                    , _returnSummary = _returnSummary a1 <> _returnSummary a2
                    , _finalizerSummary = _finalizerSummary a1 <> _finalizerSummary a2
                    , _escapeSummary = _escapeSummary a1 <> _escapeSummary a2
                    , _allocatorSummary = _allocatorSummary a1 <> _allocatorSummary a2
                    , _refCountSummary = _refCountSummary a1 <> _refCountSummary a2
                    , _scalarEffectSummary = _scalarEffectSummary a1 <> _scalarEffectSummary a2
                    , _errorHandlingSummary = _errorHandlingSummary a1 <> _errorHandlingSummary a2
                    , _transferSummary = _transferSummary a1 <> _transferSummary a2
                    , _sapSummary = _sapSummary a1 <> _sapSummary a2
                    , _sapPTRelSummary = _sapPTRelSummary a1 <> _sapPTRelSummary a2
                    , _symSliceSummary = _symSliceSummary a1 <> _symSliceSummary a2
                    , _infoSliceSummary = _infoSliceSummary a1 <> _infoSliceSummary a2
                    , _defectSummary = _defectSummary a1 <> _defectSummary a2
                    , _symExeSummary = _symExeSummary a1 <> _symExeSummary a2
                    }

instance Monoid AnalysisSummary where
  mempty = AnalysisSummary { _nullableSummary = mempty
                           , _outputSummary = mempty
                           , _arraySummary = mempty
                           , _returnSummary = mempty
                           , _finalizerSummary = mempty
                           , _escapeSummary = mempty
                           , _allocatorSummary = mempty
                           , _refCountSummary = mempty
                           , _scalarEffectSummary = mempty
                           , _errorHandlingSummary = mempty
                           , _transferSummary = mempty
                           , _sapSummary = mempty
                           , _sapPTRelSummary = mempty
                           , _symSliceSummary = mempty
                           , _infoSliceSummary = mempty
                           , _defectSummary = mempty
                           , _symExeSummary = mempty
                           }
#if !(MIN_VERSION_base(4,11,0))
  mappend    = (<>)
#endif

-- | Apply a function that uniformly summarizes *all* of the
-- individual analysis summaries.  Uses so far are extracting
-- diagnostics and producing module summaries.
extractSummary :: AnalysisSummary ->
                  (forall a . (HasDiagnostics a, SummarizeModule a) => a -> b)
                  -> [b]
extractSummary summ f =
  [ f (_nullableSummary summ)
  , f (_outputSummary summ)
  , f (_arraySummary summ)
  , f (_returnSummary summ)
  , f (_finalizerSummary summ)
  , f (_escapeSummary summ)
  , f (_allocatorSummary summ)
  , f (_refCountSummary summ)
  , f (_scalarEffectSummary summ)
  , f (_errorHandlingSummary summ)
  , f (_transferSummary summ)
  , f (_sapSummary summ)
  , f (_sapPTRelSummary summ)
  , f (_symSliceSummary summ)
  , f (_infoSliceSummary summ)
  , f (_defectSummary summ)    
  , f (_symExeSummary summ)  
  ]


---
instance HasDiagnostics Sym.SliceSummary
instance HasDiagnostics IF.SliceSummary
instance HasDiagnostics SymE.SymExeSummary

instance SummarizeModule Sym.SliceSummary where
  summarizeArgument _ _ = []
  summarizeFunction _ _ = []

instance SummarizeModule IF.SliceSummary where
  summarizeArgument _ _ = []
  summarizeFunction _ _ = []

instance SummarizeModule SymE.SymExeSummary where
  summarizeArgument _ _ = []
  summarizeFunction _ _ = []

instance Show Diagnostics where
  show (DS ds) = formatDiags ds
