import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO
import System.Environment
import System.Exit

import C hiding (error)
import Umka

parseFile :: String -> Arguments -> IO ()
parseFile filename args = do
  content <- readFile filename
  let (_content, warnings) = result content
      statements = map (\(Right a) -> a) $ filter filterStatements _content
      errors = map (\(Left e) -> e) $ filter (not . filterStatements) _content
      (umkaContent, umkaWarnings) = generateContent
        statements
        (argStructs  args)
        (argEnums    args)
        (argFuncs    args)
        (argFnPrefix args)
        (argExport   args)
        (argKeepUnresolved args)
      filterStatements (Right _) = True
      filterStatements (Left _)  = False
      generated = 
        "// Generated using umka-cheaders (https://github.com/roastpiece/umka-cheaders)\n"
        ++ unlines (filter (not . null) umkaContent)
      
    in do
    case argOutput args of
      Just output -> writeFile output generated
      Nothing     -> putStrLn generated

    hPutStrLn stderr $ unlines $ concatMap mapWarnings warnings
    hPutStrLn stderr $ unlines $ map ("WARNING: " ++) $ filter (not . null) umkaWarnings
    if null errors 
    then return ()
    else do
      hPutStrLn stderr $ unlines $ concatMap mapErrors $ concat errors
      exitWith $ ExitFailure 1


  where result = parseRec
        mapWarnings (warning, trace) = ["WARNING: " ++ warning , "\tnear: " ++ trace]
        mapErrors   (err, trace)     = ["ERROR: " ++ err , "\tnear: " ++ trace]
        printResult line = 
          case line of
            Left errs -> hPutStrLn stderr $ unlines $ concatMap mapErrors errs
            Right content -> print content

parseRec :: String -> ([Either ErrorStack Statement], WarningStack)
parseRec content = parsed
  where parsed =
          case getStatements <$> parse (parseAll []) content of
            Left errs -> ([Left errs], [])
            Right result -> result
        getStatements (result, _, warnings) = (result, warnings)
        parseAll acc = do
          r <- observing parseStatement
          case r of
            Left err -> return $ acc ++ [Left err]
            Right r' -> case r' of
              EOF -> return acc
              _   -> parseAll $ acc ++ [Right r']


  --                   Key   = Value
splitArg :: String -> (String, String)
splitArg ('-':arg) = let (key, value) = span (/= '=') arg
                         value' = drop 1 value
                     in (key, value')
splitArg arg = ("", arg)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> do
      help True
      exitWith $ ExitFailure 1
    _ -> return ()

  let args' = parseArgs args 
  case invalidArg args' of
    Nothing -> return ()
    Just a  -> do
      hPutStrLn stderr $ "Invalid argument provided: " ++ a
      help True
      exitWith $ ExitFailure 1
  
  case argInput args' of
    Nothing        -> do
      help True
      exitWith $ ExitFailure 1
    Just fileName  -> parseFile fileName args'


help :: Bool -> IO ()
help fail = do
  progName <- getProgName
  hPutStrLn os $ "Usage: " ++ progName ++ " [arguments] <file.h>"
  hPutStrLn os "Arguments:"
  hPutStrLn os "\t-output=<file.um>\toptional\t Output to file instead of stdout"
  hPutStrLn os "\t-fnprefix=<prefix>\toptional\t Prefix fn declarations with <prefix> (eg: -fnPrefix=ffi -> `ffi fn foo*();`)"
  hPutStrLn os "\t-nostructs\t\toptional\t Do not generate struct declarations"
  hPutStrLn os "\t-noenums\t\toptional\t Do not generate enum declarations"
  hPutStrLn os "\t-nofuncs\t\toptional\t Do not generate fn declarations"
  hPutStrLn os "\t-keepunresolved\toptional\t Do not skip declarations with unresolved types"
  where
    os = if fail then stderr else stdout


data Arguments = Arguments
  { argInput          :: Maybe String
  , argOutput         :: Maybe String
  , argFnPrefix       :: FnPrefix
  , argStructs        :: DoStructs
  , argEnums          :: DoEnums
  , argFuncs          :: DoFuncs
  , argKeepUnresolved :: KeepUnresolved
  , argExport         :: DoExport
  , invalidArg        :: Maybe String
  }
  deriving (Show)

defaultArgs = Arguments
  Nothing
  Nothing
  (FnPrefix "")
  (DoStructs True)
  (DoEnums True)
  (DoFuncs True)
  (KeepUnresolved False)
  (DoExport True)
  Nothing
  

parseArgs :: [String] -> Arguments
parseArgs argv = args
  where
    args = foldl (\a f -> f a) defaultArgs argumentCtrs
    argumentCtrs = map parseArg argv
    parseArg arg =
          case splitArg arg of
            ("output", filename)    -> \a -> a { argOutput = Just filename }
            ("fnprefix", prefix)    -> \a -> a { argFnPrefix = FnPrefix prefix }
            ("nostructs", _)        -> \a -> a { argStructs = DoStructs False }
            ("noenums", _)          -> \a -> a { argEnums = DoEnums False }
            ("nofuncs", _)          -> \a -> a { argFuncs = DoFuncs False }
            ("noexport ", _)        -> \a -> a { argExport = DoExport False }
            ("keepunresolved", _)   -> \a -> a { argKeepUnresolved = KeepUnresolved True }
            ("", filename)          -> \a -> a { argInput = Just filename }
            (x, _)                  -> \a -> a { invalidArg = Just  x }

