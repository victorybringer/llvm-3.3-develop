{-# LANGUAGE BangPatterns,ViewPatterns,ImplicitParams,ForeignFunctionInterface #-}


module LLVM.Slicing.Util.SrcLineNum where

import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IM
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS 
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Maybe 
import Data.List 
import Data.Char ( isSpace,toLower,ord,isLetter )
import Data.Text (Text)
import qualified Data.Text as T   -- (unpack)
import qualified Data.Text.IO as T  ( readFile )
--import qualified Data.Text.Lazy as L ( unpack,pack )
--import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8  as LBS 

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import Data.Attoparsec.ByteString.Lazy ( Parser )
import qualified Data.Attoparsec.ByteString.Lazy as P
import Control.Applicative ( many )
import Data.Word ( Word8 )
import Control.Monad ( liftM )


import Text.Printf ( printf )
import System.IO.Unsafe ( unsafePerformIO )
import System.FilePath  ( (</>) )

import Prelude hiding (readFile, writeFile)
import System.IO.Encoding
import Data.Encoding.UTF8
--import System.IO.UTF8
import System.Directory   ( doesFileExist )

import LLVM.Analysis
--import LLVM.Checking.FunctionText

import LLVM.Slicing.Util.Mix  ( groupFst,sortFst,toVarName',funcAllocInsts )



toSrcLnStr :: IntMap Value -> IntSet -> [String]    
toSrcLnStr vm = toSrcLnStr'. findVals vm
  where findVals :: IntMap Value -> IntSet -> [Value]
        findVals m ks = IM.elems (IM.filterWithKey keyF m)
          where keyF k _ = IS.member k ks  

toSrcLnStr' :: [Value] -> [String]
toSrcLnStr' = map mergSrc. groupFst. sortFst. mapMaybe valueSrcLn
  where mergSrc :: [(String,[Int])] -> String   
        mergSrc [] = ""
        mergSrc ss = hd ++ ": " ++ show lns   -- show lns ++ "@" ++ hd
         where hd = fst (head ss)
               lns = nub . sort $ concatMap snd ss    

--------
valueLine :: IsValue a => a -> [Int] 
valueLine = mapMaybe metadataToLine . valueMetadata

metadataToLine :: Metadata -> Maybe Int
metadataToLine md@MetaSourceLocation { } = Just . fromIntegral . metaSourceRow $ md
metadataToLine md@MetaDWSubprogram {} = Just . fromIntegral . metaSubprogramLine $ md
metadataToLine md@MetaDWLexicalBlock { } = Just . fromIntegral . metaLexicalBlockRow $ md
metadataToLine md@MetaDWVariable {} = Just . fromIntegral . metaGlobalVarLine $ md
metadataToLine md@MetaDWBaseType {} = Just . fromIntegral . metaBaseTypeLine $ md
metadataToLine md@MetaDWDerivedType {} = Just . fromIntegral . metaDerivedTypeLine $ md
metadataToLine md@MetaDWCompositeType {} = Just . fromIntegral . metaCompositeTypeLine $ md
metadataToLine md@MetaDWLocal {} = Just . fromIntegral . metaLocalLine $ md
metadataToLine md@MetaDWNamespace {} = Just . fromIntegral . metaNamespaceLine $ md
metadataToLine md@MetaDWTemplateTypeParameter {} = 
       Just . fromIntegral . metaTemplateTypeParameterLine $ md
metadataToLine md@MetaDWTemplateValueParameter {} = 
       Just . fromIntegral . metaTemplateValueParameterLine $ md
metadataToLine _ = Nothing
{-# INLINE metadataToLine #-} 


metadataToSrcFile :: Metadata -> Maybe FilePath
metadataToSrcFile md@MetaDWFile {} = do         
      let f = metaFileSourceFile md
          d = metaFileSourceDir md
          absSrcFile = T.unpack f -- (T.unpack d) </> (T.unpack f)
      return absSrcFile  
metadataToSrcFile md@MetaDWSubprogram {} = do     
      ctxtMD <- metaSubprogramContext md
      metadataToSrcFile ctxtMD
metadataToSrcFile md@MetaDWVariable {} = do     
      ctxtMD <- metaGlobalVarContext md
      metadataToSrcFile ctxtMD
metadataToSrcFile md@MetaDWLocal {} = do     
      ctxtMD <- metaLocalContext md
      metadataToSrcFile ctxtMD
metadataToSrcFile _ = Nothing
{-# INLINE metadataToSrcFile #-} 

--
valueSrc :: IsValue a => a -> Maybe String
valueSrc v = do 
   v' <- getValidVal v
   (fn,ln) <- valueSrcLn v' 
   n <- listToMaybe ln
   let fnStrs = map trimSpace. lines . unsafePerformIO $ let ?enc = UTF8 in readFile fn  
       vSrcLn = if n < 0 || n > length fnStrs then "" 
                else fnStrs !! (n-1)      -- R.quote $ 
       lnStr = printf ("[%s:%s] %s") fn (show n) vSrcLn
   return lnStr 
-- where 
--   trimSpace = reverse .  dropWhile isSpace .  reverse . dropWhile isSpace

getValueSrc, getValueSrc2 :: IsValue a => a -> String
getValueSrc v = 
  case getValueSrc' v of  
    Just (src,_) -> BS.unpack src
    Nothing  -> ""

getValueSrc2 = BS.unpack . BS.unlines . getValueSrc2'

getValueSrc2' v = 
  case getValueSrc' v of  
    Just (_,srcs) ->  srcs
    Nothing  -> []

getValueSrc' :: IsValue a => a -> Maybe (BS.ByteString, [BS.ByteString])
getValueSrc' v = do 
   v' <- getValidVal v
   (fn,ln) <- valueSrcLn' v' 
   n <- listToMaybe ln
   func <- valueFunction v
--   (fn',fln) <-  getFunctionLine func
   let srcMap = genSrcMap fn
       vSrcLn = getSrcByLine srcMap n 
       fln = valueLine func
       vSrcLns = if null fln then [vSrcLn] else getSrcByLines srcMap [head fln .. n]
--       lnStr = printf ("[%s:%s] %s") fn (show n) vSrcLn
   return $! (vSrcLn, vSrcLns) 

getValueBody :: IsValue a => a -> Maybe String
getValueBody v = do 
   v' <- getValidVal v
   (fn,ln) <- valueSrcLn' v' 
   n <- listToMaybe ln
   let bs =  unsafePerformIO $! LBS.readFile fn  
       isFile = unsafePerformIO $! doesFileExist fn
   if isFile then do
      body <- getAnyBodyAt n bs
      return $! BS.unpack body 
   else return ""


--
valueSrcLn, valueSrcLn' :: IsValue a => a -> Maybe (FilePath, [Int])   -- [Int]
valueSrcLn v = do
  func <- valueFunction v
  let mds = filter isSubprogramMetadata $ functionMetadata func
      line = valueLine v
  case mds of
    [md] -> do
      srcFile <- metadataToSrcFile md
      return (srcFile, line)  
    _ -> Nothing

valueSrcLn' v = do
  func <- valueFunction v
  let mds = filter isSubprogramMetadata $ functionMetadata func
      line = valueLine v
  case mds of
    [md] -> do
      srcFile <- metadataToSrcFile md
      return (srcFile, line) 
    _ -> Nothing


valueFunction :: IsValue a => a -> Maybe Function
valueFunction v = case valueContent v of
    FunctionC f -> Just f
    ArgumentC a -> Just (argumentFunction a)
    BasicBlockC b -> Just (basicBlockFunction b)
    InstructionC i -> instructionFunction i
    _ -> Nothing


----
genFuncSrcMap :: Module -> IntMap (IntMap String)
genFuncSrcMap m = 
    IM.fromList [ (valueUniqueId f, fSrcs f)  | f <- funcs]
  where  
    funcs = moduleDefinedFunctions m
    fSrcs = snd . getFuncSrcLines

genFuncLinesMap :: Module -> IntMap [Int]
genFuncLinesMap m = 
    IM.fromList [ (valueUniqueId f, fLines f)  | f <- funcs]
  where  
    funcs = moduleDefinedFunctions m
    fLines = fst . getFuncSrcLines

getFuncSrcLines :: Function -> ([Int], IntMap String)
getFuncSrcLines f = fromMaybe ([], mempty) $ do
    (fn, fBeginLn) <- getFunctionLine f
    let fileSrcMap = genSrcMap fn
        fLines = [fBeginLn .. fLastLine]
        fSrcs = IM.map LBS.unpack $
            IM.filterWithKey (\k _ -> elem k fLines) fileSrcMap
    return $ (fLines, fSrcs)
  where  
    fLastLine = maximum. concatMap valueLine $ fExitVals f
    fExitVals  = map toValue . functionExitInstructions
   

-- 
getFunctionLine :: Function -> Maybe (FilePath, Int)
getFunctionLine func = do
  let mds = filter isSubprogramMetadata $ functionMetadata func
  case mds of
    [md] -> do
      let line = metaSubprogramLine md
      srcFile <- metadataToSrcFile md
      return (srcFile, fromIntegral line)  
    _ -> Nothing


getValidVal v = case valueContent' v of 
  InstructionC RetInst {retInstValue = Nothing } -> Nothing
  InstructionC RetInst {retInstValue = Just (valueContent -> ConstantC{})} -> Nothing
  InstructionC UnconditionalBranchInst { } -> Nothing 
  InstructionC UnreachableInst { } -> Nothing
  InstructionC FenceInst { } -> Nothing   
  _  -> Just v

isSubprogramMetadata :: Metadata -> Bool
isSubprogramMetadata MetaDWSubprogram {} = True
isSubprogramMetadata _ = False

isFileSrcMetadata :: Metadata -> Bool
isFileSrcMetadata MetaDWFile {} = True
isFileSrcMetadata _ = False


isPrefix :: String -> String -> Bool
str1 `isPrefix` str2 = 
  map toLower str1 == map toLower (take (length str1) str2)


getTypeDefSrcs :: FilePath -> String -> IntMap String
getTypeDefSrcs file ty = IM.map LBS.unpack srcs
  where 
    smap = genSrcMap file
    srcs = IM.filter (filtF . LBS.toStrict) smap
    filtF s = BS.isPrefixOf (BS.pack ty) s  || 
              BS.isInfixOf (BS.pack $ ty ++ " ") s 

getVarDefSrcWith :: FilePath -> String -> [String] -> IntMap String
getVarDefSrcWith file ty vars = IM.map LBS.unpack srcs
  where 
    smap = genSrcMap file
    srcs = IM.filter (filtF . LBS.toStrict) smap
    vars' = vars ++ map (takeWhile (/= '@') . drop 1) vars
    filtF s = BS.isPrefixOf (BS.pack ty) s  && isIns vars' s 
    isIns vas s = or $ map (isIn s) vas
    isIn s name = BS.isInfixOf (BS.pack $ " " ++ name) s  ||
                  BS.isInfixOf (BS.pack $ "*" ++ name) s


getAllUnsignedSrc :: FilePath -> IntMap String
getAllUnsignedSrc fn = getTypeDefSrcs fn "unsigned"

getGlbUnsignedSrc :: Module -> FilePath -> IntMap String
getGlbUnsignedSrc m fn = IM.map LBS.unpack srcs
  where
    -- m = fn2mod fn
    funcs = moduleDefinedFunctions m
    fLines f = fromMaybe mempty $ do
      (_, fBeginLn) <- getFunctionLine f
      let fLastLine = maximum. concatMap valueLine $ fExitVals f
          fExitVals  = map toValue . functionExitInstructions          
      return $ IS.fromList [fBeginLn .. fLastLine]
    allFunLines = IS.unions $ map fLines funcs 
    smap = IM.filterWithKey (\k _ -> IS.notMember k allFunLines) $! genSrcMap fn
    srcs = IM.filter (filtF . LBS.toStrict) smap
    filtF s = BS.isPrefixOf (BS.pack "unsigned") s  || 
              BS.isInfixOf (BS.pack $ "unsigned" ++ " ") s 

----
chkVarTypeWith :: Map String (IntMap String) -> String -> Bool
chkVarTypeWith srcM var = 
  case M.lookup var srcM of 
    Just res -> not $ IM.null res
    Nothing  -> False

genUnsignedVarsMap :: Module -> Map String (IntMap String)
genUnsignedVarsMap = genVarTypeDefMapWith ["unsigned","UINT8","UINT16","UINT32","UINT64",
                      "VOS_UINT32","UCHAR","ULONG","USHORT","WORD","DWORD","BYTE"]

genVarTypeDefMapWith :: [String] -> Module -> Map String (IntMap String)
genVarTypeDefMapWith ty m = 
    M.fromList $ filter (not . IM.null . snd) 
        [(var, getUnsigned var) | var <- allVars]
  where 
    (glbUMap, funUMap) = genTypeDefSrcMapWith ty m
    getUnsigned var = IM.map BS.unpack res
      where (vName, fName) = break (=='@') var
            vName' = drop 1 vName
            glbName = drop 1 fName
            -- fName' = fName ++ "[" 
            fUnSrcs = M.findWithDefault mempty fName funUMap
            filtF1 = chkVar glbName
            filtF2 = chkVar vName'
            glbUnSrcs = IM.unionWith mrgBSs (IM.filter filtF1 glbUMap)
                        $ IM.filter filtF1 fUnSrcs 
            varUnSrcs = IM.filter filtF2 fUnSrcs
            res = if null vName then glbUnSrcs else varUnSrcs
    
    mrgBSs s1 s2 = BS.concat [s1, BS.pack "\n", s2]
    chkVar name s = BS.isInfixOf (BS.pack $ " " ++ name) s  ||
                    BS.isInfixOf (BS.pack $ "*" ++ name) s

    allVars = filter (\(_:k:_) -> isLetter k || k == '_') $
              map toVarName' allVarVals
    allVarVals = globalVals ++ concatMap localVals (moduleDefinedFunctions m)
    localVals fn = map toValue (functionParameters fn)
                   ++ map toValue (funcAllocInsts fn)
    globalVals = map toValue (moduleGlobalVariables m)
               ++ map toValue (moduleExternalValues m)


getVarTypeDefSrcWith :: Module -> [String] -> String -> IntMap String
getVarTypeDefSrcWith m ty var = varUnsigned
  where 
    (glbUMap, funUMap) = genTypeDefSrcMapWith ty m
    varUnsigned = IM.map BS.unpack res
      where (vName, fName) = break (=='@') var
            vName' = drop 1 vName
            glbName = drop 1 fName
            -- fName' = fName ++ "[" 
            fUnSrcs = M.findWithDefault mempty fName funUMap
            filtF1 = chkVar glbName
            filtF2 = chkVar vName'
            glbUnSrcs = IM.unionWith mrgBSs (IM.filter filtF1 glbUMap)
                        $ IM.filter filtF1 fUnSrcs 
            varUnSrcs = IM.filter filtF2 fUnSrcs
            res = if null vName then glbUnSrcs else varUnSrcs    
    mrgBSs s1 s2 = BS.concat [s1, BS.pack "\n", s2]
    chkVar name s = BS.isInfixOf (BS.pack $ " " ++ name) s  ||
                    BS.isInfixOf (BS.pack $ "*" ++ name) s



genUnsignedMap :: Module -> (IntMap BS.ByteString, Map String (IntMap BS.ByteString))
genUnsignedMap = genTypeDefSrcMapWith ["unsigned","UINT8","UINT16","UINT32","UINT64",
                      "VOS_UINT32","UCHAR","ULONG","USHORT","WORD","DWORD","BYTE"]

genTypeDefSrcMapWith :: [String] -> Module -> 
                        (IntMap BS.ByteString, Map String (IntMap BS.ByteString))
genTypeDefSrcMapWith tys m = (glbUMap, funUMap)
  where
    funcs = moduleDefinedFunctions m
    go f (fm,lm,sm) = fromMaybe (fm,lm,sm) $ do
        (fn, fBeginLn) <- getFunctionLine f
        let fLastLine = maximum. concatMap valueLine $ fExitVals f
            fExitVals  = map toValue . functionExitInstructions  
            fName = toVarName' f -- ++ "[" ++ fn ++ ":" ++ show fBeginLn ++ "]"
            flns = IS.fromList [fBeginLn .. fLastLine]
            fSrcs fsm = IM.map LBS.toStrict . IM.filter isUnginged $!
                 IM.filterWithKey (\k _ -> IS.member k flns) fsm
            fnSM = genSrcMap fn
            fm' = M.insert fn fnSM fm
            lm' = M.insertWith IS.union fn flns lm
            sm' fsm = let srcm = fSrcs fsm in
                      if IM.null srcm then sm
                      else M.insertWith (IM.unionWith mrgBSrc) fName srcm sm                      
        case M.lookup fn fm of
          Just fsm  -> return (fm, lm', sm' fsm) 
          Nothing   -> return (fm', lm', sm' fnSM) 
    (fnM,flnM,funUMap) = foldr go (mempty,mempty,mempty) funcs
    isUnginged s = any (flip LBS.isPrefixOf s) $ map LBS.pack tys 
                  --  BS.isInfixOf (BS.pack $ ty ++ " ") s 
    glbUMap = IM.unionsWith mrgBSrc. map getGlbUnsign $ M.toList fnM 
    mrgBSrc s1 s2 = BS.concat [s1, BS.pack "\n", s2]
    getGlbUnsign (fn,srcm) = fromMaybe mempty $ do
        lnsm <- M.lookup fn flnM
        let smap = IM.filterWithKey (\k _ -> IS.notMember k lnsm) srcm
            srcs = IM.map LBS.toStrict $! IM.filter isUnginged smap
        return $ srcs



----
genSrcMap :: FilePath -> IntMap ByteString
genSrcMap fn =  unsafePerformIO $! do
   isExit <- doesFileExist fn
   if isExit then do
      fnStrs <- LBS.readFile fn
      let fnSrcs = map trimSpace2 $ LBS.lines fnStrs 
          lnSrcMap = IM.fromList $ zip [1..] fnSrcs            
      return lnSrcMap  
   else return mempty

getSrcByLines :: IntMap ByteString -> [Int] -> [BS.ByteString]
getSrcByLines smap lns = 
   map LBS.toStrict res   -- LBS.unlines
  where bss = IM.filterWithKey (\k _ -> elem k lns) smap
        res = if null lns then [] else IM.elems bss

getSrcByLine  :: IntMap ByteString -> Int -> BS.ByteString
getSrcByLine smap n  = LBS.toStrict $! IM.findWithDefault LBS.empty n smap

--
trimSpace2 :: ByteString -> ByteString
trimSpace2 = LBS.reverse . LBS.dropWhile (== ';'). LBS.dropWhile isSpace . LBS.reverse . LBS.dropWhile isSpace

trimSpace :: String -> String
trimSpace = reverse . dropWhile (== ';') . dropWhile isSpace . reverse . dropWhile isSpace

getAnyBodyAt :: Int -> ByteString -> Maybe BS.ByteString
getAnyBodyAt n bs = liftM LBS.toStrict . P.maybeResult $! P.parse (isolator n) bs



-----------------
-- From Foreign.Inference.Report.FunctionText 

ascii :: Char -> Word8
ascii = fromIntegral . ord

isEndOfLine :: (Eq a, Num a) => a -> Bool
isEndOfLine w = w == 13 || w == 10

ignoreLine :: Parser ()
ignoreLine = do
  P.skipWhile (not . isEndOfLine)
  _ <- P.satisfy isEndOfLine
  return ()


isolator :: Int  -> Parser ByteString
isolator line = do
  _ <- P.count (line - 1) ignoreLine
  _ <- P.takeWhile (/= ascii '{')
  body <- matchedBraces
  _ <- P.takeLazyByteString
  return (toLazyByteString body)

matchedBraces :: Parser Builder
matchedBraces = do
  _ <- P.word8 (ascii '{')
  content <- many contentAndSubBody
  _ <- P.word8 (ascii '}')
  return $ mconcat [ fromString "{"
                   , mconcat content
                   , fromString "}"
                   ]

contentAndSubBody :: Parser Builder
contentAndSubBody = do
  pfx <- P.takeWhile (\c -> c /= ascii '{' && c /= ascii '}')
  P.choice [ nest pfx, blockEnd pfx ]
  where
    blockEnd :: BS.ByteString -> Parser Builder
    blockEnd pfx =
      case BS.null pfx of
        True -> fail "fail"
        False -> return (fromByteString pfx)
    nest pfx = do
      body <- matchedBraces
      rest <- contentAndSubBody
      return $ mconcat [ fromByteString pfx
                       , body
                       , rest
                       ]