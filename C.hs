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

newtype Parser a = Parser {
  parse :: String -> Either ErrorStack (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser mapP
    where mapP buff = do
            (a, rest) <- p buff
            return (f a, rest)

instance Applicative Parser where
  pure a = Parser (\buff -> Right (a, buff))

  (Parser pfab) <*> (Parser pa) = Parser f
    where f buffer = do
            (fab, buff') <- pfab buffer
            (a, buff'') <- pa buff'
            return (fab a, buff'')

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
            (first, rest) <- p1 buff
            case faMb first of
              Parser p2 -> p2 rest

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
  | SChar
  | UChar
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

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser pf
  where pf buff = Right $ span f buff

ps :: Parser String
ps = spanParser isSpace

parseWS :: Parser String
parseWS = spanParser isWS
  where isWS c
          | c /= '\n' && c /= '\r' = isSpace c
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
  where statements = parseTypeDecl <|> parseStructDecl <|> parseEnumDecl <|> parseFnPtrDecl <|> parseFnDecl
        semic = charParser ';'

        parseTypeDecl = do
          _ <- parseString "typedef" <* ps
          _ <- optional $ (parseString "struct" <|> parseString "struct") <* ps
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
          _ <- semic <* ps
          return $ FunctionDecl $ Fn ( name, retType, args )


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
      | p x = Right (x, xs)
      | otherwise = error $ "Unexpected '" ++ [x] ++ "'"
    f _ = error "Unexpected end of input"

charParser :: Char -> Parser Char
charParser c = Parser f
  where
    f (x:xs)
      | x == c = Right (c, xs)
      | otherwise = error $ "Expected " ++ [c] ++ " but got " ++ [x]
    f _ = error $ "Expected '" ++ [c] ++ "' but reached end of input"

parseCharsAsum :: String -> Parser Char
parseCharsAsum chars = asum $ map charParser chars


type ParseResult a = Either ErrorStack (a, String)

error :: Error -> ParseResult a
error msg = Left [(msg, "")]

markErrorP :: String -> Parser a -> Parser a
markErrorP msg (Parser p) = Parser markF
  where mark  buff = "near: " ++ take 40 buff
        markF buff = case p buff of
          Left stack -> Left $ stack ++ [(msg, mark buff)]
          good       -> good


type ErrorWithMark = (String, String)
type ErrorStack = [ErrorWithMark]

  
type Error = String
markError :: String -> Either Error a -> Either Error a
markError buffer (Left err) = Left $ err ++ "near: " ++ take 30 buffer
markError _ (Right r) = Right r

