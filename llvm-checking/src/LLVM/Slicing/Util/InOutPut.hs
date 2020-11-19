module LLVM.Slicing.Util.InOutPut where


import Data.Maybe 
--import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as L ( unpack )
import Data.GraphViz 
import System.FilePath
import System.IO.Unsafe ( unsafePerformIO )
import System.Directory    -- ( doesFileExist )
import System.Process (system)
import Debug.Trace
import Text.Printf ( printf )
import Control.Concurrent
import Control.Exception
import System.Timeout
import Data.Time
import Data.Char ( isLetter )
import qualified Data.Map as M  ( fromList )

import Data.Binary
import Control.Monad ( Monad,liftM )
import Data.Monoid ( Monoid,mempty )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS   ( toChunks )
import qualified Data.ByteString.Lazy.Internal as LS  ( ByteString(..) )
--import Control.Exception    ( bracket )

import LLVM.Analysis
import LLVM.Analysis.CFG 
import LLVM.Analysis.Util.Testing
--import LLVM.Analysis.Util.Names
import LLVM.Parse


import LLVM.Slicing.Data.SliceType  ( showMapWith )
--import LLVM.Slicing.Util.ValueMap ( genValueMap )
import LLVM.Slicing.Util.SrcLineNum ( valueLine )
import LLVM.Slicing.Util.ValueTest ( isAllocaInst )
import LLVM.Slicing.Util.DefRef  ( refs )
import LLVM.Slicing.Util.Mix  -- ( memAccessBase,funcAllocInsts,toVarName' )


timeIO :: IO () -> IO ()
timeIO ioa = do
    t1 <- getCurrentTime
    ioa 
    t2 <- getCurrentTime
    putStrLn $! "\tIts runtime = " ++ show (realToFrac $ diffUTCTime t2 t1)

timeIO' :: String -> IO a -> a
timeIO' ms ioa = unsafePerformIO $ do
    t1 <- getCurrentTime
    res <- ioa
    t2 <- getCurrentTime
    putStrLn $! printf ("\t%s's runtime = %s") 
                 ms (show. realToFrac $ diffUTCTime t2 t1)
    return $! res

timeIt,timeIt2 :: Show a => String -> a -> a
timeIt ms = timeIt' True ms
timeIt2 ms = timeIt' False ms

timeIt' :: Show a => Bool -> String -> a -> a
timeIt' noShow ms v = unsafePerformIO $! do
    t1 <- getCurrentTime  
    av <- return $! v
--    length (show av) `seq` putStr ""
    t2 <- getCurrentTime
    let timeStr = printf ("\n\t%s's runtime = %s")  ms
                   (show. realToFrac $ diffUTCTime t2 t1)
    putStrLn timeStr 
    appendFile "sdgDbgTime.txt" timeStr
    if noShow then return ()
    else  do
       let res = printf ("\n\t%s's result = %s") ms (show av)
       appendFile "sdgDbgTime.txt" res
--       putStrLn res 
    return av

----
timeoutWith :: Int -> IO a -> IO ()
timeoutWith sec_time action = do
    result <- timeout (10^6 * sec_time) action
    case result of
      Just result -> return ()
      Nothing     -> putStrLn $ "time limit exceeded after " ++ show sec_time ++ "sec." 

timeoutWith' :: Int -> IO () -> IO ()
timeoutWith' sec_time action = do
    tid <- myThreadId
    killer <- forkIO $ do
        threadDelay (10^6 * sec_time)
        killThread tid
    ret <- handle handleKilled action
    killThread killer
    return ()
  where
    handleKilled ThreadKilled = 
        putStrLn $ "time limit exceeded after " ++ show sec_time ++ "sec."
    handleKilled e = throwIO e

----
getFilesFromDirs :: [FilePath] -> IO [FilePath]
getFilesFromDirs dirs = do
   files <- fmap concat . sequence $! map getDirectoryContents' dirs
   let btfs = filter (flip elem [".c",".cpp",".bc",".ll"] . takeExtension) files
   return btfs
 where
   getDirectoryContents' p = do
       fns <- getDirectoryContents p
       return $! map ( p </> ) fns

getAllFiles :: [FilePath] -> IO [FilePath]
getAllFiles args = do   
   dirfs <- getFilesFromDirs dirs
   return $ files ++ dirfs
 where
   dirs = filter isDir args
   files = filter isFile args
   isDir dir = unsafePerformIO $! doesDirectoryExist dir
   isFile file = unsafePerformIO $! doesFileExist file

----
getModuleFromFile :: FilePath -> IO Module
getModuleFromFile fn = buildModule ["-g"] optOptions toModule fn
   where optOptions = ["-gvn", "-basicaa"]     -- ["-mem2reg", "-gvn"] 
         toModule = parseLLVMFile defaultParserOptions

fn2mod, fn2mod' :: FilePath -> Module
--file2Module = fn2mod"
fn2mod = file2ModuleWith ["-g"] ["-gvn", "-basicaa"]
fn2mod' = file2ModuleWith ["-g"] []

fn2modWith = file2ModuleWith
file2ModuleWith bo opt = unsafePerformIO . buildModule bo opt (parseLLVMFile defaultParserOptions)

--------
dbgIt :: (Monad m, Show b, Show a, IsValue a) => a -> m b -> m b
dbgIt i cmd = do
  res <- cmd
  let dbgStr = printf ("(%s)%s \"%s\": \n  refs = %s \n  Its_Result = %s \n")
           (show $ valueUniqueId i)(show $ valueLine i) (show i) (show $ refs i) (show res)
      dbgIO = unsafePerformIO $ do
              appendFile "myDebugIt.txt" dbgStr
              return dbgStr
  return res `debug` dbgIO

dbgInfo :: Instruction -> String
dbgInfo i = case instructionFunction i of
  Nothing -> "Err: No Function for the inst. " ++ (show i) ++ " !" 
  Just iFun -> 
    let lnStr = "  " ++ show (valueLine i) ++ show (valueUniqueId i) ++ ":  " ++ show i   -- ++ "\n"
        enStr = "Now entring the function " ++ fnName ++ "... \n" ++ lnStr
        exStr = lnStr ++ "\nLeaving from the function " ++ fnName ++ "."
        fnName = identifierAsString $ functionName iFun
    in if i == functionEntryInstruction iFun then exStr     -- enStr
       else if i `elem` functionExitInstructions iFun then enStr   -- exStr
            else lnStr 

debug = flip trace            
            
printInfo :: Module -> IO ()
printInfo m = putStrLn info 
  where 
    fs = moduleDefinedFunctions m
    allBlocks = concatMap functionBody fs
    allInsts = concatMap basicBlockInstructions allBlocks
    ctrInsts = mapMaybe ctrCondValue allInsts
    allocInsts = filter isAllocaInst allInsts        
    allVals =  map toValue (concatMap functionParameters fs)
            ++ map toValue (moduleGlobalVariables m) 
            ++ map toValue (concatMap funcAllocInsts fs)
    allVals' = filter ((\(k:_)->(isLetter k || k == '_')). drop 1. toVarName') allVals
    fnSize = "\n\t#Defined_Functions = " ++ (show $ length fs)
    cfgSize = "\n\tCFG(#Nodes,#Edges) = " ++ show (map cfgInfo fs) 
    blkSize = "\n\t#BasicBlocks = " ++ (show $ length allBlocks)  
    instSize = "\n\t#Insts_All = " ++ (show $ length allInsts)
    ctrSize = "\n\t  #Insts_br = " ++ (show $ length ctrInsts)
    varSize = "\n\t  #Insts_alloc = " ++ (show $ length allocInsts)
    varSliced = "\n\t#Vars_sliced = " ++ show (length allVals')
    info = concat ["\nIts some statistic Info.:",
            fnSize,cfgSize,blkSize,instSize,varSize,ctrSize,varSliced,"\n\n"]

ctrCondValue :: Instruction -> Maybe (Instruction,Value)
ctrCondValue i@BranchInst {branchCondition = c} = Just (i,c)
ctrCondValue i@SwitchInst {switchValue = v} = Just (i,v)
ctrCondValue i@IndirectBranchInst {indirectBranchAddress = a} = Just (i,memAccessBase a)
ctrCondValue i@SelectInst {selectCondition = c} = Just (i,c)
ctrCondValue _  = Nothing
    
cfgInfo :: Function -> String  -- (#Nodes,#Edges)
cfgInfo f = fname ++ ": " ++ show n_e
  where
    n_e = (1 + length is, length es)
    cfg = getCFG f
    fname = identifierAsString $ functionName f
    blocks = functionBody f
    is = functionInstructions f
--    ns = (-1, ()) : map (\i -> (instructionUniqueId i, ())) is
    es = concatMap (blockEdges cfg) blocks
    --
    blockEdges :: (HasCFG cfg) => cfg -> BasicBlock -> [(UniqueId, UniqueId, ())]
    blockEdges cfg b =
      addSuccessorEdges internalEdges
      where
        mkEdge s d = (s, d, ())
        is = map instructionUniqueId $ basicBlockInstructions b
        ti = instructionUniqueId $ basicBlockTerminatorInstruction b
        succs = map blockEntryId $ basicBlockSuccessors cfg b
        internalEdges = map (\(s, d) -> mkEdge s d) (zip is (tail is))
        addSuccessorEdges a
          | null succs = mkEdge ti (-1) : a
          | otherwise = map (\sb -> mkEdge ti sb) succs ++ a 
    --        
    blockEntryId :: BasicBlock -> UniqueId
    blockEntryId bb = instructionUniqueId ei
      where  ei : _ = basicBlockInstructions bb  


printValueMap :: Module -> IO ()
printValueMap m = putStrLn vmStrs
  where
    vmStrs = showMapWith "\n IR_ID      IR_Value  " id valMap
    valMap = M.fromList [(showID v, showVal v)| v <- allValues m]
    showID = show . valueUniqueId
    showVal v = case valueContent' v of
       FunctionC f   -> [takeWhile (/= '{') $ show f]
       BasicBlockC b -> [takeWhile (/= '\n') $ show b]
       _  -> [show v]
       

--
--- | for Data.GraphViz          
viewGraph :: ToGraphviz b => a -> b -> a
viewGraph v a = unsafePerformIO $ do
      let dg = toGraphviz a            
      runGraphvizCanvas' dg Gtk
      return v


showGraph :: ToGraphviz a => b -> (a,FilePath,String) -> b
showGraph v (g,fn,tys) = unsafePerformIO $ do
   let dg = toGraphviz g
       s = if null tys then "X" else tys
       dotfn = if null fn then concat ["TempX_",s, ".dot"] 
               else concat [takeBaseName fn, "_",s,".dot"]
       pngfn = takeBaseName dotfn ++ ".png"
   putStr $ concat ["\nWriting its ",s, " graph to the file ", dotfn, " ... "]
   writeFile dotfn (L.unpack $ printDotGraph dg)
   putStr " Done! \n"
--   runGraphviz dg Png pngfn 
   return v

   
showGraph' :: ToGraphviz a => b -> (a,String) -> b
showGraph' v (g,str) = unsafePerformIO $ do
   let dg = toGraphviz g
       dotfn = if null str then "MyGraph.dot" 
               else takeFileName str ++ ".CG.dot"
       pngfn = if null str then "MyGraph.png" 
               else takeFileName str ++ ".CG.png"
   putStr $ "Writing its graph to the file " ++ dotfn ++ " ... "
   writeFile dotfn (L.unpack $ printDotGraph dg)
   putStr " Done! \n"
--   runGraphviz dg Png pngfn 
   return v


printGr :: FilePath -> String -> String -> IO ()
printGr filename gtype contents = do
   writeFile dotfile contents 
   system $"dot -Tpng " ++ dotfile ++ " -o " ++ pngfile
   return ()
  where 
   dotfile = takeBaseName filename ++ "_" ++ gtype ++ ".dot"
   pngfile = takeBaseName filename ++ "_" ++ gtype ++ ".png"
--   unsuffix = reverse.tail.dropWhile (/= '.').reverse

printGr' filename gtype = writeFile dotfile 
  where 
   dotfile = takeBaseName filename ++ "_" ++ gtype ++ ".dot" 

-------------------------------------------------------
----
cachedData :: Binary a => FilePath -> IO a -> IO a
cachedData fp m = do
  exists <- doesFileExist fp
  if exists  then decodeFile fp
  else do  vm <- m
           encodeFile fp vm
           return vm
           
cachedIt :: Binary a => FilePath -> a -> a
cachedIt fp m = unsafePerformIO $ do
  exists <- doesFileExist fp
  if exists  then decodeFile fp
  else do  -- vm <- m
           encodeFile fp m
           return m

getCachedData :: Binary a => FilePath -> Maybe a
getCachedData fp = unsafePerformIO $! do
  exists <- doesFileExist fp
  if not exists then return Nothing
  else liftM Just (decodeFile fp)

getCache :: (Monoid a, Monad m, Binary a) => FilePath -> m a
getCache fp = return $! getCacheWith fp mempty
  
getCacheWith :: (Binary a) => FilePath -> a -> a
getCacheWith fp v0 = unsafePerformIO $ do
  exists <- doesFileExist fp
  if not exists then do
      putStrLn $ fp ++ " is not exists! "
      return v0
  else do  
    v <- BS.readFile fp 
    if BS.null v then do
      putStrLn $ fp ++ " is null! "
      return v0
    else do
      putStrLn $ "begin to read cache data from " ++ fp
      return $! decode2 v  
{-# INLINE getCacheWith #-}

setCachedData :: (Monad m,Binary a,Monoid a,Eq a) => FilePath -> a -> m ()
setCachedData fp m = return . unsafePerformIO $!
--  if m == mempty then return () else
  seq (BS.writeFile fp $! encode2 m) (return ())
       >> putStrLn ("write cache data to " ++ fp)

--encode = runPut. Put;  decode = runGet get

encode2 :: Binary a => a -> BS.ByteString
encode2 = lazyToStrict . encode

decode2 :: Binary a => BS.ByteString -> a
decode2 = decode . strictToLazy

-- Citing from Binary.Strict
strictToLazy :: BS.ByteString -> LS.ByteString
strictToLazy x                         -- BS.fromStrict
  | BS.null x = LS.Empty
  | otherwise = LS.Chunk x LS.Empty

lazyToStrict :: LS.ByteString -> BS.ByteString
lazyToStrict = BS.concat . LS.toChunks        -- LS.toStrict
{-# INLINE lazyToStrict #-}
