{-# LANGUAGE NoMonomorphismRestriction #-}


module LLVM.Slicing.Util.ValueTest where

import Data.Char ( isDigit )

import qualified Data.Text as T
import Data.Maybe

import LLVM.Analysis




-- Test for External Functions
isMemcpy = isExtFuns ["@llvm.memcpy.p0i8.p0i8.i32","@llvm.memcpy.p0i8.p0i8.i64"]
isMemmove = isExtFuns ["@llvm.memmove.p0i8.p0i8.i32","@llvm.memmove.p0i8.p0i8.i64"]
isMemset = isExtFuns ["@llvm.memset.p0i8.i32","@llvm.memset.p0i8.i64"]
isMemCMS = isExtFuns ["@llvm.memcpy.p0i8.p0i8.i32","@llvm.memcpy.p0i8.p0i8.i64",
                      "@llvm.memmove.p0i8.p0i8.i32","@llvm.memmove.p0i8.p0i8.i64",
                      "@llvm.memset.p0i8.i32","@llvm.memset.p0i8.i64" ]
isC99Scanf = isExtFuns ["@__isoc99_scanf","@scanf"]    -- "@__isoc99_fscanf"
isC99Read = isExtFuns ["@fread","@_IO_getc","@gets","@getw"]
isC99Printf = isExtFuns ["@printf"]    
isC99Output = isExtFuns ["@fputc","@putchar","@puts","@putw","@fwrite"]

isStrcpy = isExtFuns ["@strcpy"]
isFree = isExtFuns ["@free"]
isRemove = isExtFuns ["@remove"]

isExtFuns :: IsValue a => [String] -> a -> Bool
isExtFuns memFuns v  =
  case valueContent' (toValue v) of
    ExternalFunctionC ExternalFunction { externalFunctionName = fname } ->
      (show fname) `elem` memFuns
    _ -> False 

--- Test for a value of struct or array type
isAggregate :: (IsValue v) => v -> Bool              
isAggregate v = isGetElem v || isAggType v

isGetElem, isAggType :: (IsValue v) => v -> Bool
isGetElem v =  case valueContent' v of 
     InstructionC GetElementPtrInst {} -> True
     _ -> False
    
isAggType v =  case valueType v of
     TypeArray _ _ -> True
     TypeStruct _ _ _ -> True
     _ -> False    


----
isPointer :: (IsValue v) => v -> Bool
isPointer v = case valueType v of
  TypePointer _ _ -> True
  _ -> False  

isConstant :: Value -> Bool
isConstant v = case valueContent' v of
  ConstantC _ -> True
  _ -> False


isInstruction :: Value -> Bool
isInstruction v = case valueContent' v of
  InstructionC _ -> True
  _ -> False

isAllocaValue :: Value -> Bool
isAllocaValue v = case valueContent' v of
  InstructionC AllocaInst {} -> True
  _ -> False

hasExtraReference :: Value -> Bool
hasExtraReference v =  case valueContent v of   
  FunctionC f -> True
  InstructionC AllocaInst {} -> True
  InstructionC CallInst {} -> True
  InstructionC InvokeInst {} -> True
  GlobalVariableC _ -> True
  _  -> False

isConstantValue :: Value -> Bool
isConstantValue v =  case valueContent' v of
    ConstantC ConstantPointerNull {} -> True
    ConstantC ConstantValue {} -> True
    _ -> False

isConstantZero :: Value -> Bool
isConstantZero v =
  case valueContent' v of
    ConstantC ConstantInt { constantIntValue = 0 } -> True
    ConstantC UndefValue {}  -> True
    ConstantC ConstantPointerNull {} -> True
    ConstantC ConstantAggregateZero {} -> True
    _ -> False

isConstantUndef :: Value -> Bool
isConstantUndef v =
  case valueContent' v of
    ConstantC UndefValue {}  -> True
--    ConstantC ConstantPointerNull {} -> True
    _ -> False
    
isConstantInt :: Value -> Bool
isConstantInt v =
  case valueContent' v of
    ConstantC ConstantInt {} -> True
    _ -> False

isConstInlineAsm :: Value -> Bool
isConstInlineAsm v =
  case valueContent' v of
    ConstantC InlineAsm {} -> True
    _ -> False
    
isPointerValue :: Value -> Bool
isPointerValue v = isPointer v && 
  (not (hasExtraReference v) || isPointerPointerType v)

isPointerPointerType :: Value -> Bool
isPointerPointerType v = case valueType v of
  TypePointer (TypePointer _ _) _ -> True
  _ -> False

isFuncPtrType :: Type -> Bool
isFuncPtrType t =
  case t of
    TypeFunction _ _ _ -> True
    TypePointer t' _ -> isFuncPtrType t'
    _ -> False

isPointerType :: Type -> Bool
isPointerType v = case v of
  TypePointer _ _ -> True
  _ -> False
    
isIntType :: Type -> Bool
isIntType (TypeArray _ ty) = isIntType ty
isIntType (TypeVector _ ty) = isIntType ty
isIntType (TypeInteger _) = True
isIntType _  = False

isFpDbType :: Type -> Bool
isFpDbType (TypeArray _ ty) = isFpDbType ty
isFpDbType (TypeVector _ ty) = isFpDbType ty
isFpDbType TypeFloat = True
isFpDbType TypeDouble = True
isFpDbType _ = False

isConstantType :: Type -> Bool
isConstantType (TypeInteger _) = True
isConstantType (TypeArray _ ty) = isConstantType ty
isConstantType (TypeVector _ ty) = isConstantType ty
isConstantType TypeFloat = True
isConstantType TypeDouble = True
isConstantType _ = False

isGlobal :: Value -> Bool
isGlobal v = case valueContent v of
  GlobalVariableC _ -> True
  ExternalValueC _ -> True
  _ -> False

isConstantPointerNull :: Value -> Bool
isConstantPointerNull v =
  case valueContent' v of
    ConstantC ConstantPointerNull {} -> True
    _ -> False

isFunction :: IsValue a => a -> Bool
isFunction v =  
  case valueContent' (toValue v) of   
    FunctionC _ -> True
    _  -> False

isExtFunction :: IsValue a => a -> Bool
isExtFunction v  =
  case valueContent' (toValue v) of
    ExternalFunctionC _ -> True
    _ -> False 

--
isBBEntryInst :: Instruction -> Bool
isBBEntryInst i = 
  case instructionBasicBlock i of
    Nothing -> False
    Just iBB -> [i] == (take 1 $ basicBlockInstructions iBB)

isFunEntryInst :: Instruction -> Bool
isFunEntryInst i = 
  case instructionFunction i of
    Nothing -> False
    Just iFun -> i == functionEntryInstruction iFun

isLocalToFunction :: IsValue a => Function -> a -> Bool
isLocalToFunction f v = case valueContent v of
    FunctionC f0 -> f == f0
--    ArgumentC a -> argumentFunction a == f
    BasicBlockC b -> basicBlockFunction b == f
    InstructionC i -> instructionFunction i == Just f
    _ -> False

isCtrInst :: Instruction -> Bool
isCtrInst i = case i of
    BranchInst {} -> True
    SwitchInst {} -> True
    IndirectBranchInst {} -> True
--    UnconditionalBranchInst {} -> True
    _  -> False

isCallInst :: Instruction -> Bool
isCallInst CallInst {} = True
isCallInst InvokeInst {} = True
isCallInst _ = False  
         
isStoreInst :: Instruction -> Bool
isStoreInst StoreInst {} = True
isStoreInst _  = False

isAllocaInst :: Instruction -> Bool
isAllocaInst AllocaInst {} = True
isAllocaInst _ = False

isSwitchInst :: Instruction -> Bool
isSwitchInst SwitchInst {} = True
isSwitchInst _ = False

isDivRemInst :: Instruction -> Bool
isDivRemInst DivInst {} = True
isDivRemInst RemInst {} = True
isDivRemInst _ = False

isTransInst :: Instruction -> Bool
isTransInst TruncInst{}  = True
isTransInst ZExtInst{}  = True
isTransInst SExtInst{}  = True
isTransInst FPExtInst{}  = True
isTransInst _  = False

isTransValue :: Value -> Bool
isTransValue v =
  case fromValue v of 
    Just i   -> isTransInst i   
    Nothing -> False

isPhiNode :: IsValue a => a -> Bool
isPhiNode v =
  case valueContent' v of
    InstructionC PhiNode {} -> True
    _ -> False

isICmpVal :: IsValue a => a -> Bool
isICmpVal v =
  case valueContent' v of
    InstructionC ICmpInst {} -> True
    _ -> False

isValidVar :: IsValue a => a -> Bool
isValidVar val =
  case valueContent' val of
    GlobalVariableC _ -> True
    InstructionC AllocaInst {} -> True    
    InstructionC PhiNode {} -> True    -- new
    ArgumentC _ -> True
    GlobalAliasC _ -> True
    _  -> False
    
isTempVar :: IsValue a => a -> Bool 
isTempVar = maybe False (isDigit. head. drop 1. show). valueName    


isStructType :: Type -> Bool
isStructType t =
  case t of
    TypeStruct (Right _) _ _ -> True
    TypePointer (TypeStruct (Right _ ) _ _) _ -> True
    _ -> False      


------
---
isUnionPointerType :: Type -> Bool
isUnionPointerType t =
  case t of
    TypePointer (TypeStruct (Right name) _ _) _ ->
      T.isPrefixOf (T.pack "union.") name
    _ -> False
    
isUnsignedParam :: Argument -> Bool
isUnsignedParam a =
  fromMaybe False $ takeFirst (argumentMetadata a) $ \md -> do
    MetaDWLocal { metaLocalType = Just lt } <- return md
    case lt of
      MetaDWBaseType { metaBaseTypeEncoding = DW_ATE_unsigned } -> return True
      MetaDWDerivedType { metaDerivedTypeParent = Just baseType } ->
        case baseType of
          MetaDWBaseType { metaBaseTypeEncoding = DW_ATE_unsigned } -> return True
          _ -> return False    --  fail "Not unsigned"
      _ -> return False    --  fail "Not unsigned"

isUnsignedValue :: IsValue a => a -> Bool
isUnsignedValue a =
  fromMaybe False $ takeFirst (valueMetadata a) $ \md -> do
    MetaDWLocal { metaLocalType = Just lt } <- return md
    case lt of
      MetaDWBaseType { metaBaseTypeEncoding = DW_ATE_unsigned } -> return True
      MetaDWDerivedType { metaDerivedTypeParent = Just baseType } ->
        case baseType of
          MetaDWBaseType { metaBaseTypeEncoding = DW_ATE_unsigned } -> return True
          _ -> return False    --  fail "Not unsigned"
      _ -> return False    --  fail "Not unsigned"

isUnsignedFunctionReturn :: Function -> Bool
isUnsignedFunctionReturn f =
  fromMaybe False $ takeFirst (functionMetadata f) $ \md -> do
    MetaDWSubprogram { metaSubprogramType = ftype } <- return md
    MetaDWCompositeType { metaCompositeTypeMembers = ms } <- ftype
    MetadataList _ (Just rt : _) <- ms
    case rt of
      MetaDWDerivedType { metaDerivedTypeParent =
        Just MetaDWBaseType { metaBaseTypeEncoding = DW_ATE_unsigned }} -> return True
      MetaDWBaseType { metaBaseTypeEncoding = DW_ATE_unsigned } -> return True
      _ -> return False    --  fail "Not unsigned"


isMDSpecialType :: DW_ATE -> Metadata -> Bool
isMDSpecialType dwt md@MetaDWBaseType {} = metaBaseTypeEncoding md == dwt
isMDSpecialType dwt MetaDWDerivedType {metaDerivedTypeParent = Just md} = isMDSpecialType dwt md
isMDSpecialType dwt MetaDWLocal {metaLocalType = Just md} = isMDSpecialType dwt md
isMDSpecialType dwt MetaDWVariable {metaGlobalVarType = Just md} = isMDSpecialType dwt md
isMDSpecialType dwt (MetadataList _ (Just md : _)) = isMDSpecialType dwt md
isMDSpecialType dwt MetaDWCompositeType {metaCompositeTypeMembers = Just md} = isMDSpecialType dwt md
isMDSpecialType dwt MetaDWSubprogram {metaSubprogramType = Just md} = isMDSpecialType dwt md
isMDSpecialType dwt MetaDWTemplateValueParameter {metaTemplateValueParameterType = Just md} = isMDSpecialType dwt md
isMDSpecialType dwt MetaDWTemplateTypeParameter {metaTemplateTypeParameterType  = Just md} = isMDSpecialType dwt md
isMDSpecialType dwt _ = False 

isValueSpecialType :: IsValue a => DW_ATE -> a -> Bool
isValueSpecialType dwt val = 
  case valueMetadata val of 
    []      -> False
    md : _  -> isMDSpecialType dwt md

isSigned = isValueSpecialType DW_ATE_signed 
isSignedChar = isValueSpecialType DW_ATE_signed_char 
isSignedFixed = isValueSpecialType DW_ATE_signed_fixed 
isUnsigned = isValueSpecialType DW_ATE_unsigned 
isUnsignedChar = isValueSpecialType DW_ATE_unsigned_char 
isUnsignedFixed = isValueSpecialType DW_ATE_unsigned_fixed 
isPackedDecimal = isValueSpecialType DW_ATE_packed_decimal 

{-
data DW_ATE
  = DW_ATE_address
  | DW_ATE_boolean
  | DW_ATE_float
  | DW_ATE_decimal_float
  | DW_ATE_complex_float
  | DW_ATE_imaginary_float
  | DW_ATE_signed
  | DW_ATE_signed_char
  | DW_ATE_unsigned
  | DW_ATE_unsigned_char
  | DW_ATE_signed_fixed
  | DW_ATE_unsigned_fixed
  | DW_ATE_packed_decimal
  | DW_ATE_numeric_string
  | DW_ATE_edited
-}

--- some utils
takeFirst :: [b] -> (b -> Maybe a) -> Maybe a
takeFirst [] _ = Nothing
takeFirst (x:xs) f =
  case f x of
    Nothing -> takeFirst xs f
    j -> j