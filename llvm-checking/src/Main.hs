{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, FlexibleContexts,ScopedTypeVariables #-}
{-# OPTIONS_GHC -rtsopts #-}

module Main ( main ) where

import AI.SVM.Simple

import Control.Applicative
import Control.Exception ( tryJust )
import Control.Lens ( (.~), (^.), view )
import Control.Monad ( guard, liftM, foldM )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import Data.Maybe -- ( mapMaybe )
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LText
import qualified Data.Text.Lazy.Builder.RealFloat as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.List -- ( foldl',find, (\\), union, partition, nub, stripPrefix )
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.Char ( isLetter )

import Data.Generics.Uniplate.Data (universeBi)
import Control.Monad.Trans.State.Strict

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM

import Options.Applicative
import System.Environment ( getEnv, getArgs )
import System.FilePath ( takeBaseName )
import System.Directory ( createDirectoryIfMissing )
import System.IO.Error ( isDoesNotExistError )
import System.IO.Unsafe ( unsafePerformIO )

import Codec.Archive
import qualified ABI.Itanium as ABI

import LLVM.Analysis
import LLVM.Analysis.CFG
import LLVM.Analysis.CDG
import LLVM.Analysis.CallGraph
import LLVM.Analysis.Dominance
import LLVM.Analysis.ClassHierarchy
import LLVM.Analysis.BlockReturnValue
import LLVM.Analysis.NoReturn
import LLVM.Analysis.CallGraphSCCTraversal
import LLVM.Analysis.UsesOf
import LLVM.Analysis.Util.Testing
import LLVM.Analysis.Util.Names
import qualified LLVM.Analysis.PointsTo.TrivialFunction as PT
import LLVM.Analysis.PointsTo.Andersen
import LLVM.Analysis.PointsTo
import LLVM.Analysis.AccessPath
import LLVM.Analysis.NullPointers
import LLVM.Analysis.Dataflow

import LLVM.Parse

--import Foreign.Inference.Interface.Metadata 
import Foreign.Inference.Diagnostics
import Foreign.Inference.Interface
import Foreign.Inference.Interface.Types
import Foreign.Inference.Report
import Foreign.Inference.Preprocessing

import LLVM.Checking.Inference.Metadata
import LLVM.Checking.Inference.Allocator  as Al
import LLVM.Checking.Inference.Array  as Ar
import LLVM.Checking.Inference.ErrorHandling  as EH
import LLVM.Checking.Inference.Escape   as Es
import LLVM.Checking.Inference.Finalize  as Fi
import LLVM.Checking.Inference.Nullable  as Nu
import LLVM.Checking.Inference.Output  as Out
import LLVM.Checking.Inference.RefCount  as RC
import LLVM.Checking.Inference.Return  as Re
import LLVM.Checking.Inference.SAP  as Sap
import LLVM.Checking.Inference.SAPPTRel as SP
import LLVM.Checking.Inference.ScalarEffects  as SE
import LLVM.Checking.Inference.Transfer  as Tr
import LLVM.Checking.Inference.IndirectCallResolver  as ICR
--import qualified LLVM.Checking.Inference.INullPtrs as INP

import LLVM.Checking.CheckSummary  as ChkSumm
import LLVM.Checking.Defect as De
import LLVM.Executing.SymbolicExe as SymE
import LLVM.Executing.SymExeType as SymE

import LLVM.Slicing 
import LLVM.Slicing.Static.Symbolic.BwdSymSli as Bwd
import LLVM.Slicing.Static.Symbolic.SymSlicer0 as Sym
import LLVM.Slicing.Static.InfoFlow.InfoFlowSlicer as IF 

import Debug.Trace
import Data.SBV  as SBV   -- hiding (isSigned,isUnsigned)
-- debug = flip trace

-- zyz
zz = analyzeDefects "../test/test.cpp" m  -- printSummResult
zz2 = printSummResult m1 
zz3 = printSummResult m3 
zyz = do
  m <- getModuleFromFile "../test/sum3.c"
  analyzeSymExecuting m
  -- printSummResult m

main = do  
  ps <- getArgs  
  files <- if null ps 
           then getAllFiles ["../test/test2.cpp","../test/hw20/"]
           else getAllFiles ps
  sequence_ $ fmap cmd files 
 where
   cmd fn = do 
       putStrLn $ "Now checking defects for " ++ fn ++ "......" 
       m <- getModuleFromFile fn  
       putStrLn . showUnsignedVarMap $ genUnsignedVarsMap m
       analyzeDefects fn m

main0 = do
  ps <- getArgs
  files <- if null ps then return ["../test/test2.cpp"]
           else getAllFiles ps
  sequence_ $ fmap cmd files
 where
   cmd fn = do 
       putStrLn $ "Now executing symbolizely for " ++ fn ++ "......"  
       m <- getModuleFromFile fn  
       analyzeSymExecuting m
--   let !m = fn2modWith ["-g"] [] fileName
  -- analyzeDefects fileName m


main1 = do
  [fileName,srcName] <- getArgs
  realMain (myOpts fileName srcName)

main2 = realMain (myOpts "../test/time.bc" "../test/benchmark/time-1.7.tar.gz")

myOpts fn srcFile =  Opts { inputDependencies = []
                 , repositoryLocation = "/root/.cabal/share/x86_64-linux-ghc-8.2.2/foreign-inference-0.3.0"
                 , diagnosticLevel = Info
                 , librarySource = Just srcFile -- "../test/benchmark/gsl-1.15.tar.gz" --(fn ++ ".zip")
                 , reportDir = Just "../test/html"
                 , annotationFile = Nothing
                 , errorModelFile = Nothing
                 , noErrorLearning = True
                 , noGeneralizeErrorCodes = True
                 , dumpErrorFeatures = Nothing
                 , noLearnRCZero = True
                 , inputFile = fn
                 }

-- | The repository location is first chosen based on an environment
-- variable.  The command line argument, if specified, will override
-- it.  If the environment variable is not set, the command line
-- argument must be specified.
data Opts = Opts { inputDependencies :: [String]
                 , repositoryLocation :: FilePath
                 , diagnosticLevel :: Classification
                 , librarySource :: Maybe FilePath
                 , reportDir :: Maybe FilePath
                 , annotationFile :: Maybe FilePath
                 , errorModelFile :: Maybe FilePath
                 , noErrorLearning :: Bool
                 , noGeneralizeErrorCodes :: Bool
                 , dumpErrorFeatures :: Maybe FilePath
                 , noLearnRCZero :: Bool
                 , inputFile :: FilePath
                 }
          deriving (Show)

cmdOpts :: FilePath -> Parser Opts
cmdOpts defaultRepo = Opts
          <$> many (strOption
              ( long "dependency"
              <> short 'd'
              <> metavar "DEPENDENCY"
              <> help "A dependency of the library being analyzed."))
          <*> strOption
              ( long "repository"
              <> short 'r'
              <> metavar "DIRECTORY"
              <> value defaultRepo
              <> help "The directory containing dependency summaries.  The summary of the input library will be stored here. (Default: consult environment)")
          <*> option auto
              ( long "diagnostics"
              <> metavar "DIAGNOSTIC"
              <> value Warning
              <> help "The level of diagnostics to show (Debug, Info, Warning, Error).  Default: Warning" )
          <*> optional (strOption
              ( long "source"
              <> short 's'
              <> metavar "FILE"
              <> help "The source for the library being analyzed (tarball or zip archive).  If provided, a report will be generated"))
          <*> optional (strOption
              ( long "report-dir"
              <> short 'p'
              <> metavar "DIRECTORY"
              <> help "The directory in which the summary report will be produced.  Defaults to the REPOSITORY."))
          <*> optional (strOption
              ( long "annotations"
              <> short 'a'
              <> metavar "FILE"
              <> help "An optional file containing annotations for the library being analyzed."))
          <*> optional (strOption
              ( long "svm-error-model"
              <> short 'e'
              <> metavar "FILE"
              <> help "A trained SVM model for classifying error-reporting functions.  If this is provided, the SVM classifier will be used." ))
          <*> switch
              ( long "disable-func-generalization"
              <> help "Disable error reporting function learning entirely.  This flag overrides a specified classifier." )
          <*> switch
              ( long "disable-rc-generalization"
              <> help "Do not use known error codes to flag other blocks returning the same value as reporting errors." )
          <*> optional (strOption
              ( long "dump-error-features"
              <> metavar "FILE"
              <> help "Dump error function classification feature vectors for each function in the library into the given file."))
          <*> switch
              ( long "disable-learn-rc-zero"
              <> help "Never learn the value 0 as an error code.")
          <*> argument str ( metavar "FILE" )
 

main3 :: IO ()
main3 = do
  mRepLoc <- tryJust (guard . isDoesNotExistError) (getEnv "INFERENCE_REPOSITORY")
  let repLoc = either (const repDefault) id mRepLoc   -- error "No dependency repository specified"
      repDefault = "/root/.cabal/share/x86_64-linux-ghc-8.2.2/foreign-inference-0.3.0"
      args = info (helper <*> cmdOpts repLoc)
        ( fullDesc
        <> progDesc "Infer interface annotations for FILE (which can be bitcode or llvm assembly)"
        <> header "iiglue - A frontend for the FFI Inference engine")

  execParser args >>= realMain

realMain :: Opts -> IO ()
realMain opts = do
  let name = takeBaseName (inputFile opts)
      parseOpts = case librarySource opts of
        Nothing -> defaultParserOptions { metaPositionPrecision = PositionNone }
        Just _ -> defaultParserOptions
  mm <- buildModule ["-g"] requiredOptimizations (parseLLVMFile parseOpts) (inputFile opts)
  dump opts name mm

dump :: Opts -> String -> Module -> IO ()
dump opts name m = do
  let pta = identifyIndirectCallTargets m
      cg = callGraph m pta []
      deps = inputDependencies opts
      repo = repositoryLocation opts
  baseDeps <- loadDependencies [repo] deps
  ds <- case annotationFile opts of
    Nothing -> return baseDeps
    Just af -> do
      annots <- loadAnnotations af
      return $! addManualAnnotations baseDeps annots

  classifier <- case (errorModelFile opts, noErrorLearning opts) of
    (_, True) -> return NoClassifier
    (Just emf, False) -> liftM (FeatureClassifier . classify) (load emf)
    (Nothing, False) -> return DefaultClassifier

  -- Have to give a type signature here to fix all of the FuncLike
  -- constraints to our metadata blob.
  let funcLikes :: [FunctionMetadata]
      funcLikes = map fromFunction (moduleDefinedFunctions m)
      uses = computeUsesOf m
      errCfg = defaultErrorAnalysisOptions { errorClassifier = classifier
                                           , generalizeFromReturns = not (noGeneralizeErrorCodes opts)
                                           , prohibitLearnZero = noLearnRCZero opts
                                           }
      errRes = identifyErrorHandling funcLikes ds uses pta errCfg
      res0 = (errorHandlingSummary .~ errRes) mempty
      phase1 :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
      phase1 = [ identifyReturns ds returnSummary
               , identifyOutput m ds outputSummary
                 -- Nullable will depend on the error analysis result
               , identifyNullable ds nullableSummary returnSummary
               , identifyScalarEffects scalarEffectSummary
               , identifyArrays ds arraySummary
                 -- Finalizers will depend on nullable so that error
                 -- paths don't interfere with finalizers
               , identifyFinalizers ds pta finalizerSummary
               , identifySAPPTRels ds sapPTRelSummary
               , identifySAPs ds pta sapSummary sapPTRelSummary finalizerSummary
               , identifyEscapes ds pta escapeSummary
               , identifyRefCounting ds refCountSummary finalizerSummary scalarEffectSummary
               , identifyAllocators ds pta allocatorSummary escapeSummary finalizerSummary outputSummary
               --
               , Sym.identifySlice m symSliceSummary
               , IF.identifySlice m cg infoSliceSummary
               ]
      phase1Func = callGraphComposeAnalysis phase1
      phase1Res = parallelCallGraphSCCTraversal cg phase1Func res0
      -- The transferRes includes (builds on) the phase1Res.  The
      -- transfer analysis depends on finalizers and symbolic access paths
      transferRes = identifyTransfers funcLikes cg ds pta phase1Res finalizerSummary sapSummary transferSummary
      -- Extract the diagnostics from each analysis and combine them
      diags = mconcat $ extractSummary transferRes (view diagnosticLens)
      -- Now just take the summaries
      summaries = extractSummary transferRes ModuleSummary

  let errDat = errorHandlingTrainingData funcLikes ds uses pta errCfg
  F.for_ (dumpErrorFeatures opts) (dumpCSV errDat)

  case formatDiagnostics (diagnosticLevel opts) diags of
    Nothing -> return ()
    Just diagString -> putStrLn diagString

  -- Persist the module summary
  let repoDir = case reportDir opts of 
             Nothing -> name ++ "_Reports"
             Just d  -> d
  createDirectoryIfMissing True repoDir
--  saveModule repo name deps m summaries ds
  saveModule repoDir name deps m summaries ds
  case librarySource opts of
    Nothing -> writeSummary m summaries ds repoDir
    Just archive -> writeDetailedReport m summaries ds repoDir archive

dumpCSV :: [(Value, UV.Vector Double)] -> FilePath -> IO ()
dumpCSV dat file = LBS.writeFile file (CSV.encode dat')
  where
    dat' = mapMaybe prettyName dat
    dblToTxt = LText.encodeUtf8 . LText.toLazyText . LText.realFloat
    prettyName (v, row) = do
      ident <- valueName v
      let bs = LText.encodeUtf8 $ LText.fromChunks [identifierContent ident]
      return $ bs : map dblToTxt (UV.toList row)

writeSummary :: Module -> [ModuleSummary] -> DependencySummary -> FilePath -> IO ()
writeSummary m summaries ds rDir = do
  let rep = compileSummaryReport m summaries ds
  writeHTMLSummary rep rDir

-- | Called when a source tarball was provided.  This generates and
-- writes the report for the Module in the location specified by the
-- user.
writeDetailedReport :: Module -> [ModuleSummary] -> DependencySummary -> FilePath -> FilePath -> IO ()
writeDetailedReport m summaries ds rDir fp = do
  arc <- readArchive fp
  let rep = compileDetailedReport m arc summaries ds
  writeHTMLReport rep rDir





-------------------
------- zyz
---
instRet i = do
  f <- instructionFunction i
  let brs = labelBlockReturns f
  instructionReturn brs i

instRets i = do
  f <- instructionFunction i
  let brs = labelBlockReturns f
  instructionReturns brs i


valuesAsInsts :: [Value] -> [Instruction]
valuesAsInsts = mapMaybe fromValue

--- for C++ classes 
analyzeHierarchy :: Module -> Map String (Set String)
analyzeHierarchy = classHierarchyToTestFormat . runCHA

findCallees :: Module -> Map String (Set String)
findCallees m = M.fromList $ mapMaybe (firstCalleeTargets cha) funcs
  where
    cha = runCHA m
    funcs = moduleDefinedFunctions m

firstCalleeTargets :: CHA -> Function -> Maybe (String, Set String)
firstCalleeTargets cha f = do
  case isConstructor f || isVirtualThunk f of
    True -> Nothing
    False -> do
      firstCall <- find isCallInst insts
      callees <- resolveVirtualCallee cha firstCall
      return (fname, S.fromList (map functionToDemangledName callees))
  where
    insts = functionInstructions f
    fname = functionToDemangledName f

-- fun2DName = functionToDemangledName
-- functionToDemangledName :: Function -> String
funDName :: Function -> Either String ABI.DecodedName
funDName = ABI.demangleName . identifierAsString . functionName


isVirtualThunk :: Function -> Bool
isVirtualThunk f = case funDName f of
   Left _ -> False
   Right sname ->  case sname of
      ABI.OverrideThunk _ _ -> True
      ABI.OverrideThunkCovariant _ _ _ -> True
      _ -> False    
  
    
-- | Get the value called by a Call or Invoke instruction
calledValue :: Instruction -> Value
calledValue CallInst { callFunction = v } = v
calledValue InvokeInst { invokeFunction = v } = v
calledValue i = error ("Expected Call or Invoke instruction: " ++ show i)

-- | Return True if the given call (or invoke) instruction is a call
-- to a statically known function (rather than a function pointer).
isDirectCall :: Instruction -> Bool
isDirectCall ci = isDirectCall' cv
  where
    cv = calledValue ci
    isDirectCall' :: Value -> Bool
    isDirectCall' v = case valueContent v of
      FunctionC _ -> True
      ExternalFunctionC _ -> True
      GlobalAliasC GlobalAlias { globalAliasTarget = t } -> isDirectCall' t
      InstructionC BitcastInst { castedValue = c } -> isDirectCall' c
      _ -> False    


checkIndirectTargets :: Module -> Set String
checkIndirectTargets m = foldr targetNames mempty callTargets   
  where
    fs = moduleDefinedFunctions m
    icis = filter isIndirectCallInst (concatMap functionInstructions fs)
    callees = map callFunction icis    
    ics = identifyIndirectCallTargets m
    callTargets = concatMap (indirectCallInitializers ics) callees
    
    isIndirectCallInst :: Instruction -> Bool
    isIndirectCallInst i =
      case i of
        CallInst { callFunction = cf } ->
          case valueContent' cf of
            FunctionC _ -> False
            ExternalFunctionC _ -> False
            _ -> True
        _ -> False    
    targetNames :: Value -> Set String -> Set String
    targetNames v = S.insert (identifierAsString n)
      where
        Just n = valueName v
        
        
-- | Determine if the given function is a C2 constructor or not.  C1
-- and C3 don't give us the information we want, so ignore them
isC2Constructor :: Function -> Bool
isC2Constructor f = case funDName f of
    Left _ -> False
    Right structuredName -> case universeBi structuredName of
        [ABI.C2] -> True
        _ -> False
    
isConstructor :: Function -> Bool
isConstructor f = case funDName f of
   Left _ -> False
   Right structuredName -> case universeBi structuredName of
      [ABI.C2] -> True
      [ABI.C1] -> True
      [ABI.C3] -> True
      _ -> False

-- | Strip a prefix, operating as the identity if the input string did
-- not have the prefix.
stripPrefix' :: String -> String -> String
stripPrefix' pfx s = fromMaybe s (stripPrefix pfx s)

stripNamePrefix :: String -> String
stripNamePrefix =
  stripPrefix' "struct." . stripPrefix' "class."

typeToName :: Type -> ABI.Name
typeToName (TypeStruct (Right n) _ _) =
  case parseTypeName (stripNamePrefix (T.unpack n)) of
    Right tn -> tn
    Left e -> error ("LLVM.Analysis.ClassHierarchy.typeToName: " ++ e)
typeToName t = error ("LLVM.Analysis.ClassHierarchy.typeToName: Expected named struct type: " ++ show t)

nameToString :: ABI.Name -> String
nameToString n = fromMaybe errMsg (unparseTypeName n)
  where
    errMsg = error ("Could not encode name as string: " ++ show n)


--------------
blockRetMap :: Module -> Map String Int
blockRetMap m = foldr (recordConstIntReturn brs) mempty blocks
  where
    f1 : _ = moduleDefinedFunctions m
    blocks = functionBody f1
    brs = labelBlockReturns bdl
    cfg = controlFlowGraph f1
    pdt = postdominatorTree cfg
    bdl = Bundle f1 pdt cfg

recordConstIntReturn :: BlockReturns -> BasicBlock -> Map String Int -> Map String Int
recordConstIntReturn brs bb m =
  case blockReturn brs bb of
    Just (valueContent' -> ConstantC ConstantInt { constantIntValue = iv }) ->
      M.insert (show (basicBlockName bb)) (fromIntegral iv) m
    _ -> m

data Bundle = Bundle Function PostdominatorTree CFG
instance HasFunction Bundle where
  getFunction (Bundle f _ _) = f
instance HasPostdomTree Bundle where
  getPostdomTree (Bundle _ pdt _) = pdt
instance HasCFG Bundle where
  getCFG (Bundle _ _ cfg) = cfg  

--instance HasPostdomTree Function where
--  getPostdomTree = postdominatorTree . controlFlowGraph


--
usedInCondition :: UseSummary -> Instruction -> Bool
usedInCondition useSumm i0 = evalState (go i0) mempty
  where
    go :: Instruction -> State (Set Instruction) Bool
    go i = do
      vis <- get
      case S.member i vis of
        True -> return False
        False -> do
          put $ S.insert i vis
          let uses = usedBy useSumm (toValue i)
              cmpUses = filter isCmp uses
              bitcastUses = filter isBitcast uses
          case cmpUses of
            [] -> do
              res' <- mapM go bitcastUses
              return $ or res'
            _ -> return True
    isCmp i = case i of
        ICmpInst {} -> True
        _ -> False
    isBitcast i = case i of
        BitcastInst {} -> True
        _ -> False

srcLn2Func :: Module -> IS.Key -> Function
srcLn2Func m k = fromJust $ do 
  let valMap = genSrcLineMap m
  vals <- IM.lookup k valMap
  getFunc (head vals)


-------------------------
--
defaultDepSumm :: DependencySummary
defaultDepSumm = unsafePerformIO $ loadDependencies [] []
ds = defaultDepSumm

getSummRes :: Module -> IO (AnalysisSummary, AnalysisSummary, [ModuleSummary])
getSummRes m = do
  ds <- loadDependencies [] []
  let      
      funcLikes :: [FunctionMetadata]
      funcLikes = map fromFunction (moduleDefinedFunctions m)
      
      ics = identifyIndirectCallTargets m
--      pta = PT.runPointsToAnalysis m
      cg = callGraph m ics []
      uses = computeUsesOf m  
--      valMap = genValueMap m
--      modName = getModuleName m       
        
      errCfg = defaultErrorAnalysisOptions { errorClassifier = DefaultClassifier
                                           , generalizeFromReturns = True
                                           , prohibitLearnZero = False
                                           }
      errRes = identifyErrorHandling funcLikes ds uses ics errCfg
      res0 = (errorHandlingSummary .~ errRes) mempty
      
      phase1 :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
      phase1 = [ identifyReturns ds returnSummary
               , identifyOutput m ds outputSummary
                 -- Nullable will depend on the error analysis result
               , identifyNullable ds nullableSummary returnSummary
               , identifyScalarEffects scalarEffectSummary
               , identifyArrays ds arraySummary
                 -- Finalizers will depend on nullable so that error
                 -- paths don't interfere with finalizers
               , identifyFinalizers ds ics finalizerSummary
               , identifySAPPTRels ds sapPTRelSummary
               , identifySAPs ds ics sapSummary sapPTRelSummary finalizerSummary
               , identifyEscapes ds ics escapeSummary
               , identifyRefCounting ds refCountSummary finalizerSummary scalarEffectSummary
               , identifyAllocators ds ics allocatorSummary escapeSummary finalizerSummary outputSummary
               -- for slicing
               , Sym.identifySlice m symSliceSummary
               , IF.identifySlice m cg infoSliceSummary
               -- for executing
               , SymE.identifySymExe m ics symExeSummary outputSummary
               ]
      phase1Func = callGraphComposeAnalysis phase1
      !phase1Res = parallelCallGraphSCCTraversal cg phase1Func res0
      
      transferRes = identifyTransfers funcLikes cg ds ics phase1Res finalizerSummary sapSummary transferSummary
      diags = mconcat $ extractSummary transferRes (view diagnosticLens)      
      summaries = extractSummary transferRes ModuleSummary 
  return $! (phase1Res,transferRes,summaries)
  
printSummResult :: Module -> IO ()
printSummResult m = do
  (phase1Res,transferRes,summaries) <- getSummRes m
  let valMap = genValueMap m
      pta = PT.runPointsToAnalysis m
      modName = getModuleName m      
      
      -- | identifies functions that are allocators (in the style of malloc)
      allocSumm = Al._allocatorSummary $ ChkSumm._allocatorSummary phase1Res
      -- | HashMap Argument Int:  maps each array argument to its inferred dimensionality. 
      arraySumm = Ar._arraySummary $ ChkSumm._arraySummary phase1Res
      -- | HashMap Function (Set ErrorDescriptor)
      errorSumm = EH._errorSummary $ ChkSumm._errorHandlingSummary phase1Res
      --
      escapeSumm = ChkSumm._escapeSummary phase1Res 
      escArg_IntoArg_Field = let Es.EscapeSummary g a f ia _ = escapeSumm in (a,ia,f)
      -- | HashMap Argument [Witness]
      finalSumm = Fi._finalizerSummary $ ChkSumm._finalizerSummary phase1Res
      -- | HashMap Argument [Witness]
      nullSumm = Nu._nullableSummary $ ChkSumm._nullableSummary phase1Res
      --
      outputSumm = ChkSumm._outputSummary phase1Res 
      outArg_Filed = let Out.OutputSummary a f _ = outputSumm in (a,f)
      --
      refCntSumm = ChkSumm._refCountSummary phase1Res 
      (unrefArgs,refArgs) = let RC.RefCountSummary _ u r _ _ = refCntSumm in (u,r)
      --
      sapSumm = ChkSumm._sapSummary phase1Res
      
      
      showSliTbl = showSlices2 . M.map (toSrcLnStr valMap)
      showSliWith slices title  = title ++ showSliSize slices ++ showSliTbl slices
      symSliSumm = ChkSumm._symSliceSummary phase1Res    -- transferRes
      (symBwdSliTbl, symFwdSliTbl) = Sym.getSliceTable symSliSumm valMap m  
      symBwdRes = showSliWith symBwdSliTbl "\nBackward Static SliceTable by Symbolic Method:"  
      symFwdRes = showSliWith symFwdSliTbl "\nForward Static SliceTable by Symbolic Method:"
           
      infoSliSumm = ChkSumm._infoSliceSummary phase1Res   -- transferRes
      (infoBwdSliTbl, infoFwdSliTbl) = IF.getSliceTable infoSliSumm valMap m
      infoBwdRes = showSliWith infoBwdSliTbl "\nBackward Static SliceTable by InfoFlow Method:"
      infoFwdRes = showSliWith infoFwdSliTbl "\nForward Static SliceTable by InfoFlow Method:"

      !symExeSumm = ChkSumm._symExeSummary phase1Res 
      symExeRes = showVarMapWith S.toList $! SymE._varValueSummary symExeSumm
  
--  let reportDir = modName ++ "_Reports"     -- modName
--  createDirectoryIfMissing True reportDir
--  saveModule reportDir modName [] m summaries ds
--  writeSummary m summaries ds reportDir
  
  putStrLn $ "\nAllocator summary :: ST allocatorFuncs allocatorArgs: \n" ++ show allocSumm  
  putStrLn $ "\nArray summary :: HashMap Argument Int:\n" ++ show arraySumm
  putStrLn $ "\nError handling summary :: HashMap Function (Set ErrorDescriptor):\n" ++ show errorSumm
  putStrLn $ "\nEscape summary :: (HashMap Argument (EscapeClass, Instruction),HashMap Argument (EscapeClass, Function, Int),\
               \HashMap Argument (Set (EscapeClass, AbstractAccessPath, Instruction))):\n" ++ show escArg_IntoArg_Field
  putStrLn $ "EscapeResultToTestFormat: " ++ show (escapeResultToTestFormat escapeSumm)
  putStrLn $ "\nFinalizer summary :: HashMap Argument [Witness]:\n" ++ show finalSumm
  putStrLn $ "\nNullable summary :: HashMap Argument [Witness]:\n" ++ show nullSumm
  putStrLn $ "\nInOut summary :: (Map Argument (ArgumentDirection, [Witness]),\
               \Map (Argument, Int) (ArgumentDirection, [Witness])):\n" ++ show outArg_Filed      
  putStrLn $ "OutputSummaryToTestFormat: " ++ show (outputSummaryToTestFormat outputSumm)
  putStrLn $ "\nRef Arguments :: HashMap Argument (AbstractAccessPath, [Witness]):\n" ++ show refArgs      
  putStrLn $ "RefCountSummaryToTestFormat: " ++ show (refCountSummaryToTestFormat refCntSumm)
  putStrLn $ "\nsapReturns: " ++ showFuncMapWith HS.toList (_sapReturns sapSumm)
  putStrLn $ "sapReturnContainer : " ++ showFuncMapWith HS.toList (_sapReturnContainer sapSumm)
  putStrLn $ "sapArguments : " ++showArgMapWith HS.toList (_sapArguments  sapSumm)
  putStrLn $ "sapFinalize : " ++showArgMapWith HS.toList ( _sapFinalize sapSumm)
  
--  print (escArg,escIntoArg,escField)
--  putStrLn $ "\nDiagnostics:\n" ++ show diags
  
--  putStrLn (symBwdRes ++ symFwdRes)
--  putStrLn (infoBwdRes ++ infoFwdRes)
  putStrLn (symBwdRes ++ infoBwdRes)
  putStrLn symExeRes     -- for Sym-Executing
  
 

showFuncMapWith toList = showMapWith "\n Function          Result  " toList  . M.mapKeys getFuncName 
showArgMapWith toList = showMapWith "\n Argument           Result  " toList  . M.mapKeys toVarName'
showValMapWith toList = showMapWith "\n ValueName          Result  " toList  . M.mapKeys toVarName'
showVarMapWith toList = showMapWith "\n VarName             Value  " toList  . 
                         M.mapKeys (drop 1). M.filterWithKey (\(_:k:_) _->(isLetter k || k == '_'))
                        -- . M.map (mapMaybe SBV.unliteral. S.toList)  
--showHashMapWith toList = showMapWith2 "\n ValueName          Result  " HS.toList toList  . HM.mapKeys toVarName'
showUnsignedVarMap = showMapWith "\n UnsignedVars          DefinedSrcLines  " IM.toList 

showDefectMap :: IntMap (HashSet Value) -> String
showDefectMap dm = showMapWith2 header id id sm
  where 
    header = "\n DefectTypes           SrcLineNumbers"
    sm = mapBoth keyF valF $ IM.toAscList dm
    keyF k = "DefectType_" ++ show k
    valF  = toSrcLnStr' . HS.toList 


getDivZeros' fileName = do
  m <- getModuleFromFile fileName
  let 
     fs = moduleDefinedFunctions m
     blks = concatMap functionBody fs
     insts = concatMap basicBlockInstructions blks
     dris = filter isDivRemInst insts
     sdris = filter isStoreInst insts    -- mapMaybe succInst dris
     check1 = isConstantZero . binaryRhs
     check2 = isConstantUndef . storeValue
     vs = map toValue $ filter check1 dris ++ filter check2 sdris
  return vs
  
-----
analyzeSymExecuting :: Module -> IO ()
analyzeSymExecuting m = putStrLn symExeRes
  where
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyOutput m ds outputSummary
               , SymE.identifySymExe m ics symExeSummary outputSummary
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    !res = parallelCallGraphSCCTraversal cg analysisFunc mempty 
    !symExeSumm = ChkSumm._symExeSummary res 
    symExeRes = showVarMapWith S.toList $! SymE._varValueSummary symExeSumm
 
df = analyzeDefects
analyzeDefects :: FilePath -> Module -> IO ()
analyzeDefects fn m = do
  ds <- loadDependencies [] []
  let
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyReturns ds returnSummary
               , identifyOutput m ds outputSummary
               , identifyNullable ds nullableSummary returnSummary
               , identifyEscapes ds ics escapeSummary
               , identifyFinalizers ds ics finalizerSummary
               , identifySAPPTRels ds sapPTRelSummary
               , identifySAPs ds ics sapSummary sapPTRelSummary finalizerSummary
               , SymE.identifySymExe m ics symExeSummary outputSummary
               , identifyDefects m ds defectSummary symExeSummary nullableSummary
                                                    sapPTRelSummary sapSummary 
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty 
    dfs = De._defectSummary $! ChkSumm._defectSummary res
    -- !m1 = fn2mod fn
    -- zeros = checkOtherRules m1
    -- dfs2 = IM.unionWith HS.union dfs zeros
  putStrLn $ showDefectSumm dfs
  -- writeJsonFile (fn ++ "_result.json") dfs
    
analyzeReturns :: Module -> Set String
analyzeReturns m = S.fromList $ map getFuncName nrs
  where
    nrs = runNoReturnAnalysis cg exitTest -- runIdentity (noReturnAnalysis cg exitTest)
    pta = runPointsToAnalysis m
    cg = callGraph m pta []    
    exitTest :: (Monad m) => ExternalFunction -> m Bool
    exitTest ef = return $ "@exit" == efname
     where efname = show (externalFunctionName ef)
    runNoReturnAnalysis :: CallGraph -> (ExternalFunction -> Identity Bool) -> [Function]
    runNoReturnAnalysis cg extSummary =
      let analysis :: [CFG] -> HashSet Function -> HashSet Function
          analysis = callGraphAnalysisM runIdentity (noReturnAnalysis extSummary)
          res = callGraphSCCTraversal cg analysis mempty
      in HS.toList res


analyzeAllocator :: Module -> IO (Map String (Maybe String), Map String (Set (String, ParamAnnotation)))
analyzeAllocator m = do
  ds <- loadDependencies [] []
  let
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyEscapes ds ics escapeSummary
               , identifyFinalizers ds ics finalizerSummary
               , identifyOutput m ds outputSummary
               , identifyAllocators ds ics allocatorSummary escapeSummary finalizerSummary outputSummary
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty
    aSumm = ChkSumm._allocatorSummary res
  return $! allocatorSummaryToTestFormat aSumm

analyzeOutput :: Module -> IO (Map String (Set (String, ParamAnnotation)))
analyzeOutput m = do
  ds <- loadDependencies [] []
  let
    cg = callGraph m ics []
    ics = identifyIndirectCallTargets m
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyOutput m ds outputSummary
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty
  return $! outputSummaryToTestFormat (ChkSumm._outputSummary res)


analyzeTransfer :: DependencySummary -> Module -> Map String (Set String)
analyzeTransfer ds m =
  transferSummaryToTestFormat (ChkSumm._transferSummary res)
  where
    pta = identifyIndirectCallTargets m
    cg = callGraph m pta []
    funcLikes :: [FunctionMetadata]
    funcLikes = map fromFunction (moduleDefinedFunctions m)
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyFinalizers ds pta finalizerSummary
               , identifySAPPTRels ds sapPTRelSummary
               , identifySAPs ds pta sapSummary sapPTRelSummary finalizerSummary
               ]
    pfunc = callGraphComposeAnalysis analyses
    res0 = parallelCallGraphSCCTraversal cg pfunc mempty
    res = identifyTransfers funcLikes cg ds pta res0 finalizerSummary sapSummary transferSummary

type SapSummary = (Int, String, [AccessType])
analyzeSAPs :: DependencySummary -> Module -> Map (String, String) (Set SapSummary)
analyzeSAPs ds m =
  sapArgumentResultToTestFormat (ChkSumm._sapSummary res)
  where
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyFinalizers ds ics finalizerSummary
               , identifySAPPTRels ds sapPTRelSummary
               , identifySAPs ds ics sapSummary sapPTRelSummary finalizerSummary
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty

analyzeRefCounts :: DependencySummary -> Module -> Map String String
analyzeRefCounts ds m = refCountSummaryToTestFormat (ChkSumm._refCountSummary res)
  where
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyFinalizers ds ics finalizerSummary
               , identifyScalarEffects scalarEffectSummary
               , identifyRefCounting ds refCountSummary finalizerSummary scalarEffectSummary
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty

analyzeNullable :: DependencySummary -> Module -> Map String (Set String)
analyzeNullable ds m = nullSummaryToTestFormat (ChkSumm._nullableSummary res)
  where
    pta = identifyIndirectCallTargets m
    cg = callGraph m pta []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyReturns ds returnSummary
               , identifyNullable ds nullableSummary returnSummary
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty

analyzeInstructionEscapes :: DependencySummary -> Module -> Bool
analyzeInstructionEscapes ds m =
  isJust $ instructionEscapesWith notReturn i (ChkSumm._escapeSummary res)
  where
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyEscapes ds ics escapeSummary ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty
    Just i = find isCallInst (moduleInstructions m)
    notReturn ignoreInst =
      case ignoreInst of
        RetInst {} -> True
        _ -> False  
    moduleInstructions :: Module -> [Instruction]
    moduleInstructions = concatMap functionInstructions . moduleDefinedFunctions
    isCallInst :: Instruction -> Bool
    isCallInst i =
      case i of
        CallInst {} -> True
        _ -> False

analyzeFinalize :: DependencySummary -> Module -> Map String (Set String)
analyzeFinalize ds m =
  finalizerSummaryToTestFormat (ChkSumm._finalizerSummary res)
  where
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyFinalizers ds ics finalizerSummary ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty

analyzeEscapes :: DependencySummary -> Module -> Map String (Set (EscapeClass, String))
analyzeEscapes ds m =
  escapeResultToTestFormat (ChkSumm._escapeSummary res)
  where
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyEscapes ds ics escapeSummary ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty

analyzeArrays :: DependencySummary -> Module -> Map (String, String) Int
analyzeArrays ds m =
  arraySummaryToTestFormat (ChkSumm._arraySummary res)
  where
    pta = identifyIndirectCallTargets m
    cg = callGraph m pta []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyArrays ds arraySummary ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty

analyzeAllocator' :: DependencySummary -> Module -> AllocatorSummary
--                 -> (Map String (Maybe String), Map String (Set (String, ParamAnnotation)))
analyzeAllocator' ds m = ChkSumm._allocatorSummary res
--  allocatorSummaryToTestFormat (ChkSumm._allocatorSummary res)
--  formatDiagnostics Warning . Al._allocatorDiagnostics $ ChkSumm._allocatorSummary res
  where
    ics = identifyIndirectCallTargets m
    cg = callGraph m ics []
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyEscapes ds ics escapeSummary
               , identifyFinalizers ds ics finalizerSummary
               , identifyOutput m ds outputSummary
               , identifyAllocators ds ics allocatorSummary escapeSummary finalizerSummary outputSummary
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty

analyzeOutput' :: DependencySummary -> Module -> Map String (Set (String, ParamAnnotation))
analyzeOutput' ds m =
  outputSummaryToTestFormat (ChkSumm._outputSummary res)
  where
    cg = callGraph m ics []
    ics = identifyIndirectCallTargets m
    analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
    analyses = [ identifyOutput m ds outputSummary
               ]
    analysisFunc = callGraphComposeAnalysis analyses
    res = callGraphSCCTraversal cg analysisFunc mempty

--
--- for point-to inputs
extractPTinputs :: Module -> Map String (Set String)
extractPTinputs m =
  foldr addInfo mempty ptrs  -- `viewGraph` pta
  where
    pta = runPointsToAnalysis m
    ptrs = map toValue (globalPointerVariables m) 
            ++ formals     -- ++ map toValue (functionPointerParameters m)
    formals = concatMap (map toValue . functionParameters) (moduleDefinedFunctions m)
    addInfo v r =
      let vals = pointsTo pta v
          name = toVarName' v   -- maybe "???" show ( valueName v)
      in case null vals of
        True -> r
        False ->
          let targets = map toVarName' vals -- `debug` show vals
          in M.insert name (S.fromList targets) r
          
globalPointerVariables :: Module -> [GlobalVariable]
globalPointerVariables m = filter isPointer (moduleGlobalVariables m)  
  
functionPointerParameters :: Module -> [Argument]
functionPointerParameters m = concatMap pointerParams (moduleDefinedFunctions m)
  where
    pointerParams = filter isPointer . functionParameters   


---
extractFirstPath :: Module -> Map String (String, [AccessType])
extractFirstPath m = M.fromList $ map extractFirstFuncPath funcs
  where
    funcs = moduleDefinedFunctions m    
    extractFirstFuncPath :: Function -> ( String, (String, [AccessType]) )
    extractFirstFuncPath f = (show (functionName f), summ)
      where
        allInsts = concatMap basicBlockInstructions (functionBody f)
        Just firstStore = find isStore allInsts
        Just p = accessPath firstStore
        p' = abstractAccessPath p
        summ = (show (abstractAccessPathBaseType p'),
                abstractAccessPathComponents p')
        isStore :: Instruction -> Bool
        isStore StoreInst {} = True
        isStore _ = False


----
checkSwitchType i =  
  case i of 
    SwitchInst { switchValue = v, switchDefaultTarget = dt, switchCases = (map fst -> vs) } ->
       case (isTransValue v, null dvs) of 
         (True, _) -> nub $ mapMaybe valueSrcLn [toValue i, v]
         (False, False) -> dis
         _  -> []    
       where dvs = filter (isTypeMismatch v) vs
             dis = mapMaybe valueSrcLn (toValue i : dvs)
    _  -> []


       
isTypeMismatch v1 v2 =  case1 || case2
  where 
   case1 = v1ty /= v2ty
   case2 = isEnumVar v1  && v2ty == TypeInteger 32
   v1ty = simpleType (getAccessType v1)
   v2ty = simpleValueType (memAccessBase v2)

getEnums :: IsValue v => v -> [CEnum]
getEnums v = foldr collectEnums [] (valueMetadata v)


isMissDefault v = 
  case getValueBody v of 
   Just s -> not (isInfixOf "default:" s) && not (isInfixOf "default " s)
   Nothing -> False

isMissCases m i =
  case (isMissDefault i, getEnumValues m i) of 
    (True, Just evals) -> length evals /= length (switchCases i)
--    (False, Nothing) -> True
    _  -> False


getEnumValues m v = 
  case null venums of 
    True -> Nothing
    False -> Just enumVals
  where  
     venums = getEnumDef v
     m' = M.filterWithKey filtF m
     filtF k _ = isInfixOf (" " ++ k ++ " ") (head venums) 
     enumVals = head (M.elems m')

genCEnumMap :: Module -> Map String [(String,Int)]
genCEnumMap m = M.fromList $ zip enames evals
  where
    cenums = moduleInterfaceEnumerations m
    enames = map enumName cenums
    evals = map enumValues cenums

funcNullPtrs :: Function -> NullPointersSummary
funcNullPtrs f = getNullSummary (fromFunction f :: FunctionMetadata)   
funcBlkRets :: Function -> BlockReturns
funcBlkRets f = getBlockReturns (fromFunction f :: FunctionMetadata)

getNullPtrs :: Instruction -> [Value]
getNullPtrs i = 
  case instructionFunction i of 
    Just f -> nullPointersAt (funcNullPtrs f) i
    Nothing -> []

getInstRet :: Instruction -> [Value]
getInstRet i =    
  case instructionFunction i of 
    Just f -> maybeToList $! instructionReturn (funcBlkRets f) i
    Nothing -> []   

getInstRets :: Instruction -> [Value]
getInstRets i =    
  case instructionFunction i of 
    Just f -> concat . maybeToList $! instructionReturns (funcBlkRets f) i
    Nothing -> []     
     

--- Checking the index out of range of array or struct
-- | Return the innermost type and the index into that type accessed
-- by the GEP instruction with the given base and indices.
fieldDescriptor :: Value -> [Value] -> Maybe (Type, Int)
fieldDescriptor base ixs =
  case (valueType base, ixs) of
    -- A pointer being accessed as an array
    (_, [_]) -> Nothing
    -- An actual array type (first index should be zero here)
    (TypePointer (TypeArray _ _) _, (valueContent' -> ConstantC ConstantInt { constantIntValue = 0 }):_) ->
      Nothing
    -- It doesn't matter what the first index is; even if it isn't
    -- zero (as in it *is* an array access), we only care about the
    -- ultimate field access and not the array.  Raw arrays are taken
    -- care of above.
    (TypePointer t _, _:rest) -> return $ walkType t rest
    _ -> Nothing

walkType :: Type -> [Value] -> (Type, Int)
walkType t [] = error ("LLVM.Analysis.PointsTo.Andersen.walkType: expected non-empty index list for " ++ show t)
walkType t [(valueContent -> ConstantC ConstantInt { constantIntValue = iv })] =
  (t, fromIntegral iv)
walkType t (ix:ixs) =
  case t of
    -- We can ignore inner array indices since we only care about the
    -- terminal struct index.  Note that if there are no further
    -- struct types (e.g., this is an array member of a struct), we
    -- need to return the index of the array... FIXME
    TypeArray _ t' -> walkType t' ixs
    TypeStruct _ ts _ ->
      case valueContent ix of
        ConstantC ConstantInt { constantIntValue = (fromIntegral -> iv) } ->
          case iv < length ts of
            True -> walkType (ts !! iv) ixs
            False -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: index out of range " ++ show iv ++ " in " ++ show t)
        _ -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: non-constant index " ++ show ix ++ " in " ++ show t)
    _ -> error ("LLVM.Analysis.PointsTo.Andersen.walkType: unexpected type " ++ show ix ++ " in " ++ show t)


getFuncLines :: Module -> [(String, [Int])]
getFuncLines m = 
    zip (map toVarName' funcs) (map toLines fVals)
  where  
    funcs = moduleDefinedFunctions m
    fVals = [ toValue f : fExitVals f | f <- funcs ]
    fExitVals  = map toValue . functionExitInstructions
    toLines = concatMap valueLine  

getFunAt :: FilePath -> Int -> Maybe Function
getFunAt fn k = do
  let m = fn2mod fn
      lnValMap = genSrcLineMap m 
  getFunc . head $ lnValMap IM.! k

getInstsAt :: FilePath -> Int -> [Instruction]
getInstsAt fn = mapMaybe getInst . (IM.!) lnValMap
  where m = fn2mod fn
        lnValMap = genSrcLineMap m 

----
-- Debuging for test2.cpp
m1 = fn2mod "../test/test2.cpp"
m = fn2modWith ["-g"] [] "../test/test2.cpp"    -- "-mem2reg"
m2 = fn2modWith ["-g"] requiredOptimizations "../test/test2.cpp"  -- ["-mem2reg","-gvn","-basicaa","-disable-inlining"]
m3 = fn2mod "../test/sum3.c"

uses = computeUsesOf m
ics1 = identifyIndirectCallTargets m
ics2 = identifyIndirectCallTargets m1
cg = callGraph m ics1 [] 
pta1 = runPointsToAnalysis m
pta2 = runPointsToAnalysis m1
cha = runCHA m   -- resolverCHA ics

valPT :: IsValue a => a -> [Value] 
valPT = pointsTo pta1 . toValue

callPT :: Instruction -> [Value]
callPT = indirectCallTargets ics1 

-- findVal (^!)   -- findVals :: IntMap Value -> IntSet -> [Value]
idValMap = genValueMap m         -- IntMap Value      
idLnMap = genIdLineMap m         -- IntMap [Int]
lnIdMap = invIntMap idLnMap      -- IntMap [Int] 
lnValMap = genSrcLineMap m       -- IntMap [Value]    -- IM.!
lnValMap2 = genSrcLineMap m1
lnSrcMap = genLineSrcMap "../test/test.cpp"      -- IntMap ByteString
instCtrMap = genInstCtrMap m     -- IntMap ValueIds
callCtrMap = genCallCtrMap cg       -- IntMap ValueIds
enumMap = genCEnumMap m          -- Map String [(String,Int)]



fms = map fromFunction fs :: [FunctionMetadata]

fs = moduleDefinedFunctions m
fs3 = moduleDefinedFunctions m3
args = concatMap functionParameters fs
blks = concatMap functionBody fs
insts = concatMap basicBlockInstructions blks
cis = filter isCallInst insts
sis = filter isSwitchInst insts
ais = filter (not . isTempVar) $ filter isAllocaInst insts
getF = fromJust . getFunc . head . (IM.!) lnValMap
getF2 = fromJust . getFunc . head . (IM.!) lnValMap2
getI = mapMaybe getInst . (IM.!) lnValMap
getI2 = mapMaybe getInst . (IM.!) lnValMap2

g = getF 81
func3 = getF 93
fn3 = getF 87

-- b = ais !! 16
-- y = ais !! 4
-- f1 = getF 29
-- f3 = getF 64
-- f5 = getF 96
-- i1 = head $ getI 21
-- i2 = head $ getI 177
-- i1v = switchValue i1
-- i2v = switchValue i2
-- i2vs = map fst $ switchCases i2

brs = mconcat (map labelBlockReturns fs)
glbs = map toVarName' $ moduleGlobalVariables m
fmls = map toVarName' args 
locs = map toVarName' $ concatMap funcAllocInsts fs
vars = varFilter $ glbs ++ fmls ++ locs
varFilter = filter ((/="."). drop 1. take 2) 


(pres,tres,summs) = unsafePerformIO $! getSummRes m
nullSumm = Nu._nullableSummary $ ChkSumm._nullableSummary pres
ReturnSummary retSumm = ChkSumm._returnSummary pres

-- sbv-8.0 tests:

testSat2 =  sat $ do 
   [x, y, z] <- sIntegers ["x", "y", "z"]
   solve [x .> 5, y + z .< x]
           
myPredicate :: Predicate
myPredicate = forSome_ $ \x y z -> ((x :: SBool) .|| (sNot z)) .&& (y .|| (sNot z))

testSat = do 
         x <- isSatisfiable  $ myPredicate
         putStrLn $ show x  
