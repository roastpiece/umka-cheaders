module Umka where

import Data.Maybe
import Data.Map qualified as M

import C hiding (error)

--                               Generated  Warnings
generateContent :: [Statement] -> ([String], [String])
generateContent s = unzip . map (convertToUmka $ namedTypesMap s) $ filter filterStatement s

namedTypesMap :: [Statement] -> M.Map String Type
namedTypesMap stmts = M.fromList $ map prepare $ filter filterNamedT stmts
  where prepare (TypeDecl (TypedIdent (t, n))) = (n, t)
        prepare (StructDecl (name, _)) = (name, NamedType name)
        prepare (EnumDecl (name, _))   = (name, NamedType name)

        filterNamedT (TypeDecl (TypedIdent (NamedType _, _))) = False
        filterNamedT (TypeDecl (TypedIdent (_, _)))           = True
        filterNamedT (StructDecl (name, _))                   = True
        filterNamedT (EnumDecl (name, _))                     = True
        filterNamedT _                                        = False

filterStatement :: Statement -> Bool
filterStatement (FunctionDecl _) = True
filterStatement (StructDecl _)   = True
filterStatement (EnumDecl _)     = True
filterStatement (TypeDecl (TypedIdent (NamedType _, _))) = True

filterStatement _                = False

endl = "\n"
indent = "    "

convertToUmka :: M.Map String Type -> Statement -> (String, String)
convertToUmka m (FunctionDecl (Fn (name, retType, args))) =
  if (not . null) fnPtrArgs
  then ("", "Skipped function `" ++ name ++ "` because it has unsupported arguments.")
  else (gen, "")
  where fnPtrArgs = filter filterFnPtr args
        filterFnPtr VarArg = True
        filterFnPtr (TypeArg (TypedIdent (t, _))) =
          case t of
            NamedType tn ->
              case M.lookup tn m of
                Nothing         -> False
                Just (Ptr (FnType _)) -> True
                Just _          -> False
            _ -> False

        gen = "ffi fn "
          ++ escapeKw name ++ "*("
          ++ join ", " (map convertFnArg args) ++ ")"
          ++ if retType /= Void then ": " ++ convertType retType ++ ";" else ";"

convertToUmka _ (StructDecl (name, members)) = (gen, "")
  where gen = "type " ++ escapeKw name ++ "* = struct {" ++ endl
          ++ concatMap (\(TypedIdent (t,n)) -> indent ++ escapeKw n ++ ": " ++ convertType t ++ endl) members
          ++ "}"

convertToUmka _ (EnumDecl (name, members)) = (gen, "")
  where gen = "type " ++ escapeKw name ++ "* = enum {" ++ endl
          ++ concatMap (\(n,mi) -> indent ++ escapeKw n ++ maybe "" ((" = " ++) . show) mi ++ endl) members
          ++ "}"

convertToUmka m (TypeDecl (TypedIdent (t, n))) =
  if unresolvedT
  then ("", "Skippe typedef `" ++ n ++ "` because it is unresolved")
  else (gen, "")
  where gen = "type " ++ escapeKw n ++ "* = " ++ convertType t
        unresolvedT =
          case t of
            NamedType tn -> not $ tn `M.member` m
            _ -> False
          

join :: String -> [String] -> String
join _ []    = ""
join sep els = foldl1 (\acc b -> acc ++ sep ++ b) els

convertFnArg :: FnArg -> String
convertFnArg VarArg = "__varargs: ..^void"
convertFnArg (TypeArg (TypedIdent (t, name))) = escapeKw name ++ ": " ++ convertType t

convertType :: Type -> String
convertType (IntType t) = convertIntType t
convertType Real32 = "real32"
convertType Real64 = "real"
convertType Bool   = "bool"
convertType Void   = "void"
convertType (Ptr (IntType (Signed Int8)))   = "str"
convertType (Ptr (IntType (Unsigned Int8))) = "str"
convertType (Ptr t) = "^" ++ convertType t
convertType (NamedType name) = escapeKw name
convertType (Array arr) = convertArrayType arr

convertIntType :: IntType -> String
convertIntType (Signed size)   = convertIntSize size
convertIntType (Unsigned size) = "u" ++ convertIntSize size

convertIntSize :: IntSize -> String
convertIntSize Int8  = "int8"
convertIntSize Int16 = "int16"
convertIntSize Int32 = "int32"
convertIntSize Int64 = "int"

convertArrayType :: (Type,Maybe Int) -> String
convertArrayType (t, n) = "[" ++ maybe "" show n ++ "]" ++ convertType t

reservedKeywords =
  [ "type"
  , "break"
  , "case"
  , "const"
  , "continue"
  , "default"
  , "else"
  , "enum"
  , "fn"
  , "for"
  , "import"
  , "interface"
  , "if"
  , "in"
  , "map"
  , "return"
  , "str"
  , "struct"
  , "switch"
  , "type"
  , "var"
  , "weak"
  , "real"
  , "real32"
  , "str"
  ]
escapeKw :: String -> String 
escapeKw name = if name `elem` reservedKeywords
                then "__" ++ name
                else name
