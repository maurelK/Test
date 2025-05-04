{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Main file
-}
--import ParserJson
--import MyDocFormat
--
--main :: IO ()
--main = do
--  let simpleInput = "{\"header\":\"{\\\"title\\\":\\\"Simple\\\"}\", \"content\":\"[{\\\"text\\\":\\\"test\\\"}]\"}"
--  print $ parseDocument simpleInput

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Usage
import Data.Maybe (isJust, fromJust, fromMaybe)
import ParserJson (parseJsonDoc)

data Arguments = Arguments
  { input  :: Maybe FilePath
  , format :: Maybe String
  , output :: Maybe FilePath
  , inputFormat :: Maybe String
  } deriving (Show)

defaultArguments :: Arguments
defaultArguments = Arguments
  { input = Nothing
  , format = Nothing
  , output = Nothing
  , inputFormat = Nothing
  }

getOpts :: Arguments -> [String] -> Maybe Arguments
getOpts args [] = Just args
getOpts args ("-i" : val : rest) = getOpts (args { input = Just val }) rest
getOpts args ("-f" : val : rest) = getOpts (args { format = Just val }) rest
getOpts args ("-o" : val : rest) = getOpts (args { output = Just val }) rest
getOpts args ("-e" : val : rest) = getOpts (args { inputFormat = Just val }) rest
getOpts _ _ = Nothing

detectFormat :: FilePath -> String
detectFormat path =
  case path of
    ".json"     -> "json"
    ".xml"      -> "xml"
    ".md"       -> "markdown"
    _           -> "unknown"

main = do
  argv <- getArgs
  case getOpts defaultArguments argv of
    Just args
      | isJust (input args) && isJust (format args) -> do
          content <- readFile (fromMaybe "" (input args))
          let inFmt = fromMaybe (detectFormat (fromMaybe "" (input args))) (inputFormat args)
              parsedDoc = case inFmt of
                "json"     -> parseJsonDoc content
                _          -> Left "Unknown input format"
          case parsedDoc of
            Left err -> putStrLn ("Error: " ++ err) >> exitWith (ExitFailure 84)
            Right doc -> do
              let outFmt = fromJust (format args)
                  outputStr = case outFmt of
                    --"json"     -> toJsonDoc doc
                    _          -> "Unsupported output format"
              case output args of
                Just outPath -> writeFile outPath outputStr
                Nothing      -> putStrLn outputStr
      | otherwise -> usage >> exitWith (ExitFailure 84)
    Nothing -> usage >> exitWith (ExitFailure 84)
