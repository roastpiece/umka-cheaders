import Control.Applicative
import System.IO
import Data.Foldable

import C
import Umka qualified

parseFile :: IO ()
parseFile = do
  content <- readFile "raylib.h"
  let (_content, warnings) = result content
      statements = map (\(Right a) -> a) $ filter filterStatements _content
      errors = map (\(Left e) -> e) $ filter (not . filterStatements) _content
      (umkaContent, umkaWarnings) = Umka.generateContent statements
      filterStatements (Right _) = True
      filterStatements (Left _)  = False
      
    in do
    putStrLn "// Generated using umka-cheaders (https://github.com/roastpiece/umka-cheaders)"
    putStrLn $ unlines $ filter (not . null) umkaContent
    hPutStrLn stderr $ unlines $ concatMap mapWarnings warnings
    hPutStrLn stderr $ unlines $ map ("WARNING: " ++) $ filter (not . null) umkaWarnings
    hPutStrLn stderr $ unlines $ concatMap mapErrors $ concat errors
    -- hPrint stderr $ Umka.namedTypesMap statements


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


main :: IO ()
main = parseFile
