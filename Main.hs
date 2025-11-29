module Main where

import Prelude hiding (takeWhile)
import Data.List hiding (takeWhile)
import Data.Maybe
import Debug.Trace
import Data.Char
import Control.Monad
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)
import Numeric (readBin)

data IntSize
  = Int8
  | Int16
  | Int32
  | Int64
  deriving (Show, Eq)

data IntType
  = Signed IntSize
  | Unsigned IntSize
  deriving (Show, Eq)

data Type
  = IntType IntType
  | Real32
  | Real64
  | Bool
  | SChar
  | UChar
  | Void
  | Ptr
  | Array (Type, Int)
  | NamedType (String)
  deriving (Show, Eq)

data TypedIdent =
  TypedIdent (Type, String)
  deriving (Show, Eq)

data Declaration
  = FunctionDecl ( String -- name
                 , Type -- return type
                 , [TypedIdent] -- args (type, name)
                 )
  | StructDecl ( String -- name
               , [TypedIdent] -- members (type, name)
               )
  | EnumDecl   ( String -- name
               , [(String, Maybe Int)] -- members (type, name)
               )
  | TypeDecl   ( TypedIdent )
  | SkipDecl   ( String ) -- as hints

  deriving (Show, Eq)

data Token
  = Type Type
  | IntSize IntSize
  | UnsignedKeyword
  | Typedef
  | Ident (String)
  | IntLiteral (Int)
  | Semicolon
  | LPar
  | RPar
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | StructKeyword
  | EnumKeyword
  | ExternKeyword
  | Comma
  | Skip
  | EqSign
  | EOF
  deriving (Show, Eq)

tokens :: [(String, Token)]
          -- known sized
tokens = [("int8_t",   Type $ IntType $ Signed Int8)
         ,("int16_t",  Type $ IntType $ Signed Int16)
         ,("int32_t",  Type $ IntType $ Signed Int32)
         ,("int64_t",  Type $ IntType $ Signed Int64)
         ,("uint8_t",  Type $ IntType $ Unsigned Int8)
         ,("uint16_t", Type $ IntType $ Unsigned Int16)
         ,("uint32_t", Type $ IntType $ Unsigned Int32)
         ,("uint64_t", Type $ IntType $ Unsigned Int64)

         -- maybe unsigned
         ,("char",     IntSize Int8)
         ,("short",    IntSize Int16)
         ,("int",      IntSize Int32)
         ,("long",     IntSize Int64)

         --,("uint",     Type Uint32)
         --,("uchar",    Type Uint8)
         ,("float",    Type Real32)
         ,("double",   Type Real64)
         --,("bool",     Type Bool)
         ,("void",     Type Void)
         ,("*",        Type Ptr)
         ,("const",    Skip)
         ,("static",   Skip)
         ,("struct",   StructKeyword)
         ,("enum",     EnumKeyword)
         ,("extern",   ExternKeyword)
         ,("unsigned", UnsignedKeyword)
         ,("typedef",  Typedef)
         ,(";",        Semicolon)
         ,("(",        LPar)
         ,(")",        RPar)
         ,("{",        LBrace)
         ,("}",        RBrace)
         ,("[",        LBrack)
         ,("]",        RBrack)
         ,(",",        Comma)
         ,("=",        EqSign)
         ,("!",        Skip)
         ]

data PreprocessTokens
  = PP_ifndef
  | PP_ifdef
  | PP_if
  | PP_else
  | PP_elif
  | PP_endif
  | PP_include
  | PP_define


getIdent :: Token -> Either Error String
getIdent (Ident ident) = Right ident
getIdent tok = Left $ "Identifier expected but got " ++ show tok

mapToken :: String -> (String, Token) -> Maybe (Either Error (Token, String))
mapToken buffer (tokenStr, token) =
  case nextHead of
    Just name -> if tokenStr == name
                 then Just $ Right (token, drop tokLen buffer)
                 else Nothing
    Nothing -> Nothing
  where
    tokLen = length tokenStr
    nextHead = Just $ head $ groupBy ((&&) `on` isIdentChar) buffer

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

getIdentifier :: String -> Either Error (Token, String)
getIdentifier "" = Left "Error parsing identifier"
getIdentifier (c:rest)
  | isAlpha c || c == '_' =
    let (identTail, identRest) = takeWhile isIdentChar rest
        ident                  = Ident (c:identTail)
    in Right (ident, identRest)
  | otherwise = Left $ "Identifier cannot start with " ++ [c]


getNextToken :: String -> Either Error (Token, String)
getNextToken "" = Right (EOF, "")

getNextToken ('#':rest) = getNextToken $ skipUntil "\n" rest
getNextToken ('/':'/':rest) = getNextToken $ skipUntil "\n" rest
getNextToken ('/':'*':rest) = getNextToken $ skipUntil "*/" rest

getNextToken (h:b:rest)
  | isNumber h =
      case h of
        '0' -> case b of
          'x' -> readIntBased rest "0x"
          'b' -> readIntBinary rest
          'o' -> readIntBased rest "0b"
          _ -> readInt buffer
        _ -> readInt buffer

  | isSpace h = getNextToken $ dropWhile isSpace (b:rest)
  | otherwise =
  let maybeNextTok = join $ find isJust $ map (mapToken buffer) tokens
  in do
    (nextTok, rest) <- fromMaybe (getIdentifier buffer) maybeNextTok
    case nextTok of
      Skip   -> getNextToken rest
      _      -> return (nextTok, rest)
  where
    buffer = (h:b:rest)

    readIntBased buff base = let (digits, afterNumber) = takeWhile isNumber buff
                                 number :: Int = read (base ++ digits)
                             in Right (IntLiteral number, afterNumber)

    readIntBinary buff = let (digits, afterNumber) = takeWhile isNumber buff
                             [(number, _)] = readBin digits
                         in Right (IntLiteral number, afterNumber)

    readInt buff = let (digits, afterNumber) = takeWhile isNumber buff
                       number :: Int = read digits
                   in Right (IntLiteral number, afterNumber)

skipToken :: Token -> String -> Either Error String
skipToken tok buffer =
  case getNextToken buffer of
    Right (nextTok, rest) -> if nextTok == tok
                             then Right rest
                             else (markError buffer) $ Left $ (show tok) ++ " expected but got " ++ (show nextTok)
    Left err -> Left err

takeWhile :: (Char -> Bool) -> String -> (String, String)
takeWhile = takeWhileN 0

takeWhileN :: Int -> (Char -> Bool) -> String -> (String, String)
takeWhileN _ _ "" = ("", "")
takeWhileN num fn str
  | fn nextC = takeWhileN (num+1) fn str
  | otherwise = (take num str, drop num str)
  where
    nextC =
      case drop num str of
        (c:_) -> c
        _     -> '\0'
      

skipUntil :: String -> String -> String
skipUntil "" _ = ""
skipUntil _ "" = ""
skipUntil needle stack
  | len > length stack = ""
  | needle == front    = drop len stack
  | otherwise          = skipUntil needle $ drop 1 stack
  where
    len   = length needle
    front = take len stack

takeUntilStr :: String -> String -> (String, String)
takeUntilStr needle stack = takeUntilN needle stack 0

takeUntilN :: String -> String -> Int -> (String, String)
takeUntilN "" _ _ = ("", "")
takeUntilN _ "" _ = ("", "")
takeUntilN needle stack n
  | len > length stack = (stack, "")
  | needle == front    = (take (n + len) stack, drop (n+len) stack)
  | otherwise          = takeUntilN needle stack (n+1)
  where
    len   = length needle
    front = take len $ drop n $ stack

takeUntilAny :: String -> String -> (String, String)
takeUntilAny chars stack = takeUntilAnyN chars stack 0

takeUntilAnyN :: String -> String -> Int -> (String, String)
takeUntilAnyN "" _ _ = ("", "")
takeUntilAnyN _ "" _ = ("", "")
takeUntilAnyN chars buffer n
  | isJust $ find (== front!!0) chars = (take n buffer, drop n buffer)
  | otherwise                         = takeUntilAnyN chars buffer (n+1)
  where
    front = take 1 $ drop (n) buffer
  

parser :: String -> Either Error (Declaration, String)
-- skip preprocessor directives for now
-- todo: skip multiline macros

parser buffer =
  case getNextToken buffer of
    Right (nextTok, rest) -> parseNextToken nextTok rest
    Left err -> Left err

parseNextToken :: Token -> String -> Either Error (Declaration, String)
parseNextToken (Type t)  b = parseFunctionDeclaration t b
parseNextToken (Typedef) b = parseTypeDefinition b
parseNextToken (ExternKeyword) b = Right $ (SkipDecl "extern", skipUntil "\n" b)
parseNextToken nextTok   b = (markError b) $ Left $ "Unexpected token " ++ show nextTok

parseFunctionDeclaration :: Type -> String -> Either Error (Declaration, String)
parseFunctionDeclaration retType buffer = do
  (funName, restName) <- parseName buffer
  (funSig, restSig)   <- parseFunSig restName
  return (
    FunctionDecl (funName, retType, funSig)
    ,restSig
         )

mapParsed :: (a -> b) -> (a, String) -> (b, String)
mapParsed = undefined


parseType :: String -> Either Error (Type, String)
parseType buffer = do
  (nextTok, rest) <- getNextToken buffer
  (_t, afterType) <- case nextTok of
      UnsignedKeyword -> do
        (sizeTok, rest) <- getNextToken rest
        case sizeTok of
          IntSize a -> return $ ((IntType $ Unsigned a), rest)
          tok -> (markError rest) $  Left $ "Expected int type but got " ++ show tok

      IntSize a -> return $ ((IntType $ Signed a), rest)
      Type t -> return (t, rest)
      Ident name -> return $ (NamedType name, rest)
      tok ->(markError buffer) $  Left $ "Expected type but got " ++ show tok

  (tokAfterType, afterPtrCheck) <- getNextToken afterType
  (_t, afterPtr) <- case tokAfterType of
    Type Ptr -> parsePtr afterPtrCheck
    LBrack   -> parseArray _t afterType
    Ident _  -> do
      (nextTok, _) <- getNextToken afterPtrCheck
      case nextTok of
        LBrack -> do
          (_t,_) <- parseArray _t afterPtrCheck
          return (_t, afterType)
        _      -> return (_t, afterType)
    _        -> return (_t, afterType)

  return (_t, afterPtr)

  where
    parseArray :: Type -> String -> Either Error (Type, String)
    parseArray t buffer = do
      _b <- skipToken LBrack buffer
      (size, _b) <- parseNumber _b
      _b <- skipToken RBrack _b
      return (Array (t, size), _b)

    parsePtr :: String -> Either Error (Type, String)
    parsePtr buffer = do
      (nextTok, afterPtr) <- getNextToken buffer
      case nextTok of
        Type Ptr -> parsePtr afterPtr
        _        -> return $ (Ptr, buffer)

  

parseNumber :: String -> Either Error (Int, String)
parseNumber buffer = do
  (nextTok, rest) <- getNextToken buffer
  case nextTok of
    IntLiteral n -> return (n, rest)
    _            -> Left $ "IntLiteral expected but got " ++ (show nextTok)


parseName :: String -> Either Error (String, String)
parseName buffer = do
  (nextTok, afterName) <- getNextToken buffer
  name <- (markError buffer) $ getIdent nextTok

  -- skip array suffix if present
  -- this gets looked-ahead parse by type parser

  (nextTok, beforeArray) <- getNextToken afterName
  end <- case nextTok of
    LBrack -> return $ skipUntil "]" beforeArray
    _      -> return $ afterName
  
  return (name, end)

parseFunSig :: String -> Either Error ([TypedIdent], String)
parseFunSig buffer = do
  _b <- skipToken LPar buffer
  (sig, _b) <- parseFunParameters _b
  _b <- skipToken RPar _b
  _b <- skipToken Semicolon _b
  return (sig, _b)

parseTypedIdent :: String -> Either Error (TypedIdent, String)
parseTypedIdent buffer = do
  (t, rest) <- parseType buffer
  (name, rest) <- parseName rest
  return $ (TypedIdent (t, name), rest)


parseFunParameters :: String -> Either Error ([TypedIdent], String)
parseFunParameters = parseFunParametersEx []

parseFunParametersEx :: [TypedIdent] -> String -> Either Error ([TypedIdent], String)
parseFunParametersEx params buffer =
  case getNextToken buffer of
    Right (RPar, _) -> Right (params, buffer)
    Right (Comma, rest) -> parseFunParametersEx params rest
    Right _ -> do
      (ident, rest) <- parseTypedIdent buffer
      parseFunParametersEx (params ++ [ident]) rest

    Left err -> Left err


parseTypeDefinition :: String -> Either Error (Declaration, String)
parseTypeDefinition buffer = do
  (nextToken, b) <- getNextToken buffer
  case nextToken of
    StructKeyword    -> parseStructDefinition b
    EnumKeyword      -> parseEnumDefinition b

    _ -> parseNewType buffer

parseNewType :: String  -> Either Error (Declaration, String)
parseNewType buffer = do
  (nextType, afterType) <- parseType buffer
  (tokAfterType, afterNextTok) <- getNextToken afterType
  case tokAfterType of
    Ident name -> parseTypeDeclaration buffer
    LPar       -> skipDecl
  where
    (skipped, afterSkip) = takeUntilStr ";" buffer
    skipDecl = Right (SkipDecl skipped, afterSkip)


parseTypeDeclaration :: String -> Either Error (Declaration, String)
parseTypeDeclaration buffer = do
  (typedIdent, _b) <- parseTypedIdent buffer
  _b <- skipToken Semicolon _b
  return (TypeDecl typedIdent, _b)


parseStructDefinition :: String -> Either Error (Declaration, String)
parseStructDefinition buffer = do
  (_nextToken, _b) <- getNextToken buffer
  -- skip optional name before {
  _b <- case _nextToken of
    Ident _ -> return _b
    _       -> return buffer

  (_nextToken, _b) <- getNextToken _b
  case _nextToken of
    LBrace -> do
      (members, _b) <- parseStructMembers _b
      _b <- skipToken RBrace _b
      (name, _b) <- parseName _b
      _b <- skipToken Semicolon _b
      return (StructDecl(name, members), _b)
    Ident name -> do
      _b <- skipToken Semicolon _b
      return (StructDecl(name, []), _b)
    tok -> (markError _b) $ Left $ "Unexpcted token " ++ show tok ++" in struct declaration."


parseStructMembers :: String -> Either Error ([TypedIdent], String)
parseStructMembers = parseStructMembersEx []

parseStructMembersEx :: [TypedIdent] -> String -> Either Error ([TypedIdent], String)
parseStructMembersEx members buffer = do
  (member, _b) <- parseTypedIdent buffer

  (names, bufAfterMember) <- parseMultiNames [] _b
  newMembers <- return $ mapNames member names
  afterSemic <- skipToken Semicolon bufAfterMember

  (nextTok, _b) <- getNextToken afterSemic
  case nextTok of
    RBrace -> return ((members ++ newMembers), afterSemic)
    _ -> do
      parseStructMembersEx (members ++ newMembers) afterSemic

  where
    mapNames :: TypedIdent -> [String] -> [TypedIdent]
    mapNames (TypedIdent(t, name)) names = map (\n -> TypedIdent (t, n)) (name:names)

    parseMultiNames :: [String] ->  String -> Either Error ([String], String)
    parseMultiNames names buf = do
      (nextToken, _b) <- getNextToken buf
      case nextToken of
        Semicolon -> return (names, buf)
        Comma -> do
          (name, _b) <- parseName _b
          parseMultiNames (names ++ [name]) _b
        tok -> Left $ "Unexpected " ++ show tok ++ " in struct definition"


parseEnumDefinition :: String -> Either Error (Declaration, String) 
parseEnumDefinition buffer = do
  (nextToken, _b) <- getNextToken buffer
  -- skip optional name before {
  _b <- case nextToken of
    Ident _ -> skipToken LBrace _b
    _       -> skipToken LBrace buffer

  (members, _b) <- parseEnumMembers [] _b
  _b <- skipToken RBrace _b
  (name, _b) <- parseName _b
  _b <- skipToken Semicolon _b
  return (EnumDecl (name, members), _b)
      

parseEnumMembers :: [(String, Maybe Int)] -> String -> Either Error ([(String, Maybe Int)], String) 
parseEnumMembers members buffer = do
  (_nextTok, afterTok) <- getNextToken buffer
  case _nextTok of
    Ident name -> do
      (_nextTok, _b) <- getNextToken afterTok
      (number, afterMember) <-
        case _nextTok of
          EqSign -> do
            (number, _b) <- parseNumber _b
            return (Just number, _b)
          Comma -> return (Nothing, afterTok)
          RBrace -> return (Nothing, afterTok)
          tok -> (markError afterTok) $ Left $ "Unexpected " ++ (show tok) ++ " in enum definition"
      parseEnumMembers (members ++ [(name, number)]) afterMember

    Comma -> parseEnumMembers members afterTok
    RBrace -> return (members, buffer)
    tok -> (markError afterTok) $ Left $ "Unexpected " ++ (show tok) ++ " in enum definition"

markError :: String -> Either Error a -> Either Error a
markError buffer (Left err) = Left $ err ++ "\nnear: " ++ take 30 buffer
markError _ (Right r) = Right r

type Error = String

content :: String
content = unsafePerformIO $ readFile "raylib.h"

parseFile :: IO ()
parseFile = putStrLn $ unlines $ map show $ parseRec content

parseRec :: String -> [String]
parseRec content =
    let decs = []
        parsed = parser content
        nextDecs =
          case parsed of
            Left err -> decs ++ [err]
            Right (a, rest) ->
              decs ++ [show a] ++ parseRec rest
    in nextDecs


main :: IO ()
main = putStrLn "Hello world"
