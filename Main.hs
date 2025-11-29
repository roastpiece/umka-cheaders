module Main where

import Prelude hiding (takeWhile)
import Data.List hiding (takeWhile)
import Data.Maybe
import Data.Char
import Control.Monad
import Data.Function (on)

data Type
  = Sint8
  | Sint16
  | Sint32
  | Sint64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Real32
  | Real64
  | SChar
  | UChar
  | Void
  | Ptr
  | Struct (String)
  | Enum   (String)
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
  deriving (Show, Eq)

data Token
  = Type Type
  | Typedef
  | Ident (String)
  | Semicolon
  | LPar
  | RPar
  | Comma
  | Skip
  | EOF
  deriving (Show, Eq)

tokens :: [(String, Token)]
tokens = [("int8_t",   Type Sint8)
         ,("int16_t",  Type Sint16)
         ,("int32_t",  Type Sint32)
         ,("int64_t",  Type Sint64)
         ,("int",      Type Sint32)
         ,("char",     Type Sint8)
         ,("long long",Type Sint64)
         ,("long",     Type Sint64)
         ,("uint8_t",  Type Uint8)
         ,("uint16_t", Type Uint16)
         ,("uint32_t", Type Uint32)
         ,("uint64_t", Type Uint64)
         ,("uint",     Type Uint32)
         ,("uchar",    Type Uint8)
         ,("float",    Type Real32)
         ,("double",   Type Real64)
         ,("void",     Type Void)
         ,("*",        Type Ptr)
         ,("const",    Skip)
         ,("static",   Skip)
         ,("typedef",  Typedef)
         ,(";",        Semicolon)
         ,("(",        LPar)
         ,(")",        RPar)
         ,(",",        Comma)
         ]

getIdent :: Token -> Either Error String
getIdent (Ident ident) = Right ident
getIdent tok = Left $ "Identifier expected but got: " ++ show tok

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
getNextToken (h:rest)
  | isSpace h = getNextToken $ dropWhile isSpace rest
  | otherwise =
  let maybeNextTok = join $ find isJust $ map (mapToken buffer) tokens
  in do
    (nextTok, rest) <- fromMaybe (getIdentifier buffer) maybeNextTok
    case nextTok of
      Skip   -> getNextToken rest
      Type a -> do
        (tokAfterType, restAfterType) <- getNextToken rest
        if tokAfterType == Type Ptr
        then return (Type Ptr, restAfterType)
        else return (nextTok, rest)

      _      -> return (nextTok, rest)
  where
    buffer = (h:rest)

skipToken :: Token -> String -> Either Error String
skipToken tok buffer =
  case getNextToken buffer of
    Right (nextTok, rest) -> if nextTok == tok
                             then Right rest
                             else Left $ (show tok) ++ " expected but got " ++ (show nextTok)
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
  | needle == front    = (take n stack, drop n stack)
  | otherwise          = takeUntilN needle stack (n+1)
  where
    len   = length needle
    front = take len stack

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
parser "" = Left ("Unexpected EOF")

-- skip preprocessor directives for now
-- todo: skip multiline macros
parser ('#':rest) = parser $ skipUntil "\n" rest
-- line comments, skip until EOL
parser ('/':'/':rest) = parser $ skipUntil "\n" rest
-- block comments
parser ('/':'*':rest) = parser $ skipUntil "*/" rest

parser buffer =
  case getNextToken buffer of
    Right (nextTok, rest) -> parseNextToken nextTok rest
    Left err -> Left err

parseNextToken :: Token -> String -> Either Error (Declaration, String)
parseNextToken (Type t) = parseFunctionDeclaration t

parseFunctionDeclaration :: Type -> String -> Either Error (Declaration, String)
parseFunctionDeclaration retType buffer = do
  (funName, restName) <- parseFunName buffer
  (funSig, restSig)   <- parseFunSig restName
  return (
    FunctionDecl (funName, retType, funSig)
    ,restSig
         )

parseFunName :: String -> Either Error (String, String)
parseFunName buffer = do
  (nextTok, rest) <- getNextToken buffer
  name <- getIdent nextTok
  return (name, rest)

parseFunSig :: String -> Either Error ([TypedIdent], String)
parseFunSig buffer = do
  b1 <- skipToken LPar buffer
  (sig, rest) <- parseFunParameters b1
  b2 <- skipToken RPar rest
  b3 <- skipToken Semicolon b2
  return (sig, b3)

parseTypedIdent :: String -> Either Error (TypedIdent, String)
parseTypedIdent buffer =
  case getNextToken buffer of
    Right (Type t, rest) ->
      case getNextToken rest of
        Right (Ident name, rest) -> Right $ (TypedIdent (t, name), rest)
        Right (other, _) -> Left $ "Unexpected " ++ show other
        Left err -> Left err
    Right (other, _) -> Left $ "Unexpected " ++ show other
    Left err -> Left err


parseFunParameters :: String -> Either Error ([TypedIdent], String)
parseFunParameters = parseFunParametersEx []

parseFunParametersEx :: [TypedIdent] -> String -> Either Error ([TypedIdent], String)
parseFunParametersEx params buffer =
  case getNextToken buffer of
    Right (RPar, rest) -> Right (params, buffer)
    Right (Comma, rest) -> parseFunParametersEx params rest
    Right _ -> do
      (ident, rest) <- parseTypedIdent buffer
      parseFunParametersEx (params ++ [ident]) rest

    Left err -> Left err

type Error = String


main :: IO ()
main = putStrLn "Hello world"
