module C where

import Prelude hiding (takeWhile, error)
import Data.List hiding (takeWhile)
import Data.Maybe
import Data.Map qualified as M
import Debug.Trace
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)
import Numeric (readBin)

data Empty = Empty
type WarningStack = ErrorStack

--                          stack trace  parsed remaining buffer  warnings
type ParseResult a = Either ErrorStack   (a,    String,           WarningStack)
newtype Parser a = Parser {
  parse :: String -> ParseResult a
}

instance Functor Parser where
  fmap f (Parser p) = Parser mapP
    where mapP buff = do
            (a, rest, warnings) <- p buff
            return (f a, rest, warnings)

instance Applicative Parser where
  pure a = Parser (\buff -> Right (a, buff, []))

  (Parser pfab) <*> (Parser pa) = Parser f
    where f buffer = do
            (fab, buff', warnings) <- pfab buffer
            (a, buff'', warnings') <- pa buff'
            return (fab a, buff'', warnings ++ warnings')

instance Alternative Parser where
  empty = Parser f
    where f _ = Left []

  (Parser p1) <|> (Parser p2) = Parser f
    where f buff = case p1 buff of
            Left _ -> p2 buff
            good   -> good

instance Monad Parser where
  (Parser p1) >>= faMb = Parser f
    where f buff = do
            (first, rest, warnings) <- p1 buff
            (next, rest', warnings') <- case faMb first of
                                          Parser p2 -> p2 rest
            return (next, rest', warnings ++ warnings')

--scanM :: Monad m => (b -> a -> m b) -> b -> [m a] -> [m b]

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
  | Void
  | Ptr Type
  | FnType Fn
  | Array (Type, Maybe Int)
  | NamedType String
  deriving (Show, Eq)

newtype Fn = Fn ( String -- name
             , Type -- return type
             , [FnArg] -- args (type, name)
             )
  deriving (Show, Eq)

data FnArg
  = TypeArg TypedIdent
  | VarArg
  deriving (Show, Eq)

newtype TypedIdent =
  TypedIdent (Type, String)
  deriving (Show, Eq)

newtype IntLiteral = IntLiteral Int

data Statement
  = FunctionDecl Fn
  | StructDecl ( String -- name
               , [TypedIdent] -- members (type, name)
               )
  | EnumDecl   ( String -- name
               , [(String, Maybe Int)] -- members (type, name)
               )
  | TypeDecl   TypedIdent
  | SkipDecl   String  -- as hints
  | Preprocessor String
  | Comment String
  | EOF

  deriving (Show, Eq)

  
typeTokens = [("int8_t",  IntType $ Signed Int8)
            ,("int16_t",  IntType $ Signed Int16)
            ,("int32_t",  IntType $ Signed Int32)
            ,("int64_t",  IntType $ Signed Int64)
            ,("uint8_t",  IntType $ Unsigned Int8)
            ,("uint16_t", IntType $ Unsigned Int16)
            ,("uint32_t", IntType $ Unsigned Int32)
            ,("uint64_t", IntType $ Unsigned Int64)
            ,("float",    Real32)
            ,("double",   Real64)
            ,("void",     Void)
            ]

            -- maybe unsigned
intSizeTokens = [("char",     Int8)
               ,("short",     Int16)
               ,("int",       Int32)
               ,("long long", Int64)
               ,("long",      Int64)
               ]

addThd3 :: c -> (a, b) -> (a, b, c)
addThd3 c (a,b) = (a,b,c)

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser pf
  where pf buff = Right $ addThd3 [] $ span f buff


ps :: Parser String
ps = spanParser isSpace

parseUntilEol :: Parser String
parseUntilEol = spanParser isEolC
  where isEolC c
          | c /= '\n' && c /= '\r' = True
          | otherwise              = False

parseAnyC :: String -> Parser String
parseAnyC = traverse charParser

parseTokenMap :: [(String, a)] -> Parser a
parseTokenMap = asum . m
  where m = map (\(str,a) -> a <$ parseString str)

parsePtr :: Parser Type -> Parser Type
parsePtr mt = do
  baseT <- mt
  ptrCs' <- ptrCs
  foldM foldPtr baseT ptrCs'

  where ptrCs = many $ charParser '*' <* ps
        foldPtr t _ = pure $ Ptr t

parseArray :: Type -> Parser Type
parseArray t = do
  _ <- charParser '[' <* ps
  size <- optional $ (\(IntLiteral int) -> int) <$> parseIntLit <* ps
  _ <- charParser ']' <* ps
  return $ Array (t, size)

parseIntSize :: Parser IntSize
parseIntSize = parseTokenMap intSizeTokens

parseUnsignedInt :: Parser IntType
parseUnsignedInt = unsigned *> ps *> (Unsigned <$> parseIntSize)
  where unsigned = parseString "unsigned"

parseSignedInt :: Parser IntType
parseSignedInt = Signed <$> parseIntSize

parseInt :: Parser Type
parseInt = toType <$> ints
  where ints = parseUnsignedInt <|> parseSignedInt
        toType = IntType

parseFixedType :: Parser Type
parseFixedType = parseTokenMap typeTokens

parseNamedType :: Parser Type
parseNamedType = NamedType <$> parseIdentM

parseTypeN :: Parser Type
parseTypeN = do
  _ <- optional $ parseString "const" <* ps
  t <- parsePtr t
  arr <- optional $ parseArray t
  return $ fromMaybe t arr
  where types = parseFixedType <|> parseInt <|> parseNamedType
        t     = types <* ps

concatStringM :: Parser String -> String -> Parser String
concatStringM mb s = (s ++) <$> mb
  
parseIdentM :: Parser String
parseIdentM = markErrorP "Error parsing identifier" $ firstC >>= concatStringM (parseRest <* ps)
  where firstC = singleton <$> parseCharF firstCF
        firstCF '_' = True
        firstCF c   = isAlpha c
        parseRest   = spanParser restCF
        restCF '_'  = True
        restCF c    = isAlphaNum c

parseTypedIdentM :: Parser TypedIdent
parseTypedIdentM = do
  TypedIdent (t, name) <- typedIdent
  arr <- optional $ parseArray t
  return $ case arr of
             Just arr' -> TypedIdent (arr', name)
             Nothing -> TypedIdent (t, name)

  where typedIdentM t = (\name -> TypedIdent (t, name)) <$> parseIdentM
        typedIdent = (parseTypeN <* ps) >>= typedIdentM

parseStatement :: Parser Statement
parseStatement = markErrorP "Error parsing statement" $ statements <* ps
  where statements =
          parseEOF
          <|> parseComment
          <|> parsePP
          <|> parseTypeDecl
          <|> parseStructDecl
          <|> parseEmptyStructDecl
          <|> parseEnumDecl
          <|> parseFnPtrDecl
          <|> parseFnDecl

        semic = charParser ';'

        parseTypeDecl = do
          _ <- parseString "typedef" <* ps
          --_ <- optional $ (parseString "struct" <|> parseString "struct") <* ps
          (TypeDecl <$> parseTypedIdentM) <* semic <* ps

        parseStructDecl = do
          _ <- parseString "typedef" <* ps
          _ <- parseString "struct" <* ps
          _ <- optional $ parseIdentM <* ps
          _ <- charParser '{' <* ps
          members <- many $ parseStructMembers <* semic <* ps
          _ <- charParser '}' <* ps
          name <- parseIdentM <* ps
          _ <- semic <* ps
          return $ StructDecl (name, concat members)
          where parseStructMembers = do
                  TypedIdent (t, name) <- parseTypedIdentM
                  names <- many $ charParser ',' *> ps *> parseIdentM <* ps
                  return $ map (\n -> TypedIdent (t, n)) (name : names)

        parseEmptyStructDecl = do
          _ <- parseString "typedef" <* ps
          _ <- parseString "struct" <* ps
          _ <- parseIdentM <* ps
          name <- parseIdentM <* ps
          _ <- semic <* ps
          return $ StructDecl (name, [])

        parseEnumDecl = do
          _ <- parseString "typedef" <* ps
          _ <- parseString "enum" <* ps
          _ <- optional $ parseIdentM <* ps
          _ <- charParser '{' <* ps
          members <- some $ parseEnumMember <* ps
          _ <- charParser '}' <* ps
          name <- parseIdentM <* ps
          _ <- semic <* ps
          return $ EnumDecl (name, members)
          where parseEnumMember = do
                  name <- parseIdentM
                  value <-  optional $ ps *> charParser '=' *> ps *> parseIntLit <* ps
                  _ <- optional $ charParser ',' <* ps
                  return (name, (\(IntLiteral int) -> int) <$> value)

        parseFnArgs = do
          args <- many arg
          maybeVarArgs <- optional $ parseString "..." <* ps
          varArgs <- case maybeVarArgs of
                       Just _ -> pure [VarArg]
                       Nothing -> pure []
          return $ map TypeArg args ++ varArgs
          where arg = do
                  ti <- parseTypedIdentM <* ps
                  _ <- optional $ charParser ',' <* ps
                  return ti

        -- typedef void (*TraceLogCallback)(int logLevel, const char *text, va_list args);
        parseFnPtrDecl = do
          _ <- parseString "typedef" <* ps
          retType <- parseTypeN <* ps
          _ <- charParser '(' <* ps
          _ <- charParser '*' <* ps
          name <- parseIdentM <* ps
          _ <- charParser ')' <* ps
          _ <- charParser '(' <* ps
          args <- parseFnArgs
          _ <- charParser ')' <* ps
          _ <- semic <* ps
          let fn = Ptr $ FnType $ Fn ( name, retType, args )
              in return $ TypeDecl $ TypedIdent ( fn, name)

        -- void InitWindow(int width, int height, const char *title);
        -- FunctionDecl Fn
        parseFnDecl = do
          retType <- parseTypeN <* ps
          name <- parseIdentM <* ps
          _ <- charParser '(' <* ps
          args <- parseFnArgs
          _ <- if null args
               then optional $ parseString "void" <* ps
               else optional ps
          _ <- charParser ')' <* ps
          _ <- ((Empty <$ semic) <|> skipBlock) <* ps
          return $ FunctionDecl $ Fn ( name, retType, args )

        -- Preprocessor statements
        -- These get skipped for now and generate a warning
        parsePP = do
          parsePPStatement <*
            parserWarning "Preprocessor statements are ignored. Consider running the header file throug `cpp -P` first."


parseEOF = Parser f
  where f "" = pure (EOF, "", [])
        f _  = error "" ""

parseComment = parseBlockComment <|> parseLineComment
  where
    parseLineComment = do
        _ <- parseString "//"
        content <- parseUntilEol
        _ <- parseString "\n\r" <|> parseString "\n" <|> ("" <$ parseEOF)
        return $ Comment content
    parseBlockComment = do
        _ <- parseString "/*"
        content <- parseUntil (parseString "*/")
        _ <- parseString "*/"
        return $ Comment content


parseIntLit :: Parser IntLiteral
parseIntLit = (hex <|> bin <|> oct <|> dec) <* ps
  where dec = IntLiteral . read <$> spanParser isDigit
        bin = (\[(num, _)] -> IntLiteral num) . readBin <$> (parseString "0b" *> some (parseCharsAsum "01") <* ps)
        hex = fmap (IntLiteral . read) $ parseString "0x" >>= concatStringM (some hexDigit <* ps)
        oct = fmap (IntLiteral . read) $ parseString "0o" >>= concatStringM (some octDigit <* ps)
        hexDigit = parseCharF isDigit <|> parseCharsAsum "abcdefABCDEF"
        octDigit = parseCharsAsum "01234567"


parseString :: String -> Parser String
parseString token = markErrorP ("Expected " ++ token) $ traverse charParser token

parseCharF :: (Char -> Bool) -> Parser Char
parseCharF p = Parser f
  where
    f (x:xs)
      | p x = Right (x, xs, [])
      | otherwise = error ("Unexpected '" ++ [x] ++ "'") (x:xs)
    f buff = error "Unexpected end of input" buff


parseAnyChar :: Parser Char
parseAnyChar = Parser f
  where
    f (x:xs) = Right (x, xs, [])
    f buff = error "Unexpected end of input" buff

charParser :: Char -> Parser Char
charParser c = do
  x <- parseAnyChar
  if x == c
    then return x
    else parserError $ "Expected " ++ [c] ++ " but got " ++ [x]


parseCharsAsum :: String -> Parser Char
parseCharsAsum chars = asum $ map charParser chars

parseCNoSuffix :: Show a => Parser a -> Parser Char
parseCNoSuffix parseSuffix = do
  c <- parseAnyChar
  suffix <- optional parseSuffix
  case suffix of
    Just s -> do
      parserError $ "Unexpected '" ++ show s ++ "'"
    Nothing -> return c

parseUntil :: Show a => Parser a -> Parser String
parseUntil p = many (parseCNoSuffix p) >>= concatStringM (singleton <$> parseAnyChar)

parsePPStatement :: Parser Statement
parsePPStatement = do
  _ <- charParser '#'
  Preprocessor <$> parseUntil parseEoPP

  where parseEoPP = do
          continue <- optional $ charParser '\\'
          eol <- parseString "\n\r" <|> parseString "\n"
          case continue of
            Nothing -> return eol
            Just _  -> parserError ""


skipBlock :: Parser Empty
skipBlock = do
  _ <- charParser '{' <* ps
  skipToEnd

  where f :: Int -> String -> ParseResult Empty
        f 0 rest       = Right (Empty, rest, [])
        f n (x:xs)
          | x == '{'   = skipCommentsContinue (n+1) xs
          | x == '}'   = skipCommentsContinue (n-1) xs
          | otherwise  = skipCommentsContinue n xs
        f _ []         = error "Unexpected EOF" ""

        skipCommentsContinue n buff =
          case parse parseComment buff of
            Right (_, buff', _) -> f n buff'
            Left _ -> f n buff

        skipToEnd = Parser $ f 1


traceP :: String -> String
traceP = take 40

error :: Error -> String -> ParseResult a
error msg buff = Left [(msg, traceP buff)]

parserError :: String -> Parser a
parserError msg = Parser f
  where f = error msg

parserWarning :: String -> Parser Empty
parserWarning msg = Parser f
  where f buff = Right (Empty, buff, [(msg, traceP buff)])

markErrorP :: String -> Parser a -> Parser a
markErrorP msg (Parser p) = Parser markF
  where markF buff = case p buff of
          Left stack -> Left $ stack ++ [(msg, traceP buff)]
          good       -> good

observing :: Parser a -> Parser (Either ErrorStack a)
observing (Parser p) = Parser f
  where f buffer = case p buffer of
                     Right (result, rest, warnings) -> Right (Right result, rest, warnings)
                     Left err -> Right (Left err, buffer, mempty)


type ErrorWithMark = (String, String)
type ErrorStack = [ErrorWithMark]
type Error = String

