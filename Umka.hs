module Umka where

import Debug.Trace
import Data.Maybe
import Data.Map qualified as M

import C hiding (error)

generateContent
  :: String -- fn prefix
  -> Bool -- generate structs
  -> Bool -- generate enums
  -> Bool -- generate funcs
  -> Bool -- keep unresolved
  -> [Statement]
  -> ([String], [String]) --Generated  Warnings
generateContent fPrefix doStructs doEnums doFuncs keepUnresolved s = unzip . map (convertToUmka fPrefix keepUnresolved namedTypesMap') $ filteredStatements
  where
    filteredStatements = filter filterStatement s
    namedTypesMap'      = namedTypesMap filteredStatements
    filterStatement (FunctionDecl _) = doFuncs
    filterStatement (StructDecl _)   = doStructs
    filterStatement (EnumDecl _)     = doEnums
    filterStatement (TypeDecl (TypedIdent (NamedType _, _))) = doStructs || doEnums
    filterStatement _                = False

namedTypesMap :: [Statement] -> M.Map String Type
namedTypesMap = foldl buildMap M.empty
  where prepare (TypeDecl (TypedIdent (t, n))) = (n, t)
        prepare (StructDecl (name, _)) = (name, NamedType name)
        prepare (EnumDecl (name, _))   = (name, NamedType name)
        prepare _ = undefined
        buildMap acc e = if filterNamedT acc e
                         then uncurry M.insert (prepare e) acc
                         else acc

        -- TODO: register type alias -> NamedType
        filterNamedT m (TypeDecl (TypedIdent (NamedType tname, _))) = tname `M.member` m
        filterNamedT _ (TypeDecl (TypedIdent (_, _)))               = True
        filterNamedT _ (StructDecl (_, _))                          = True
        filterNamedT _ (EnumDecl (_, _))                            = True
        filterNamedT _ _                                            = False


endl = "\n"
indent = "    "

convertToUmka :: String -> Bool -> M.Map String Type -> Statement -> (String, String)
convertToUmka fPrefix keep m (FunctionDecl (Fn (name, retType, args))) =
  if (not . null) unresolved && not keep
  then ("", "Skipped function `" ++ name ++ "` because it has unsupported/unresolved arguments. Keep with `-keepunresolved`.")
  else (gen, "")
  where unresolved = filter filterUnresolved args
        filterUnresolved VarArg = True
        filterUnresolved (TypeArg (TypedIdent (t, _))) =
          case t of
            NamedType tn ->
              case M.lookup tn m of
                Nothing               -> True
                Just (Ptr (FnType _)) -> True
                Just _                -> False
            _ -> False

        gen = (if null fPrefix then "fn " else fPrefix ++ " fn ")
          ++ escapeKw name ++ "*("
          ++ join ", " (map convertFnArg args) ++ ")"
          ++ if retType /= Void then ": " ++ convertType retType ++ ";" else ";"

convertToUmka _ _ _ (StructDecl (name, members)) = (gen, "")
  where gen = "type " ++ escapeKw name ++ "* = struct {" ++ endl
          ++ concatMap (\(TypedIdent (t,n)) -> indent ++ escapeKw n ++ ": " ++ convertType t ++ endl) members
          ++ "}"

convertToUmka _ _ _ (EnumDecl (name, members)) = (gen, "")
  where gen = "type " ++ escapeKw name ++ "* = enum {" ++ endl
          ++ concatMap (\(n,mi) -> indent ++ escapeKw n ++ maybe "" ((" = " ++) . show) mi ++ endl) members
          ++ "}"

convertToUmka _ keep m (TypeDecl (TypedIdent (t, n))) =
  if unresolvedT && not keep
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
