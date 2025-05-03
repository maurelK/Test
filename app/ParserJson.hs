{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Parser file
-}

module ParserJson where

import MyDocFormat
import Data.Maybe

skipWhitespace :: String -> String
skipWhitespace str = dropWhile isWhitespace str

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\t' || c =='\n' || c == '\r'


-- Ceci est à revoir
parseString :: String -> Maybe (String, String)
parseString ('"':xs) = Just (takeWhile (/= '"') xs, drop (length (takeWhile (/= '"') xs) + 1) xs)
parseString _ = Nothing

startsWith :: Char -> String -> Bool
startsWith _ [] = False
startsWith c (x:xs) = c == x

parseObject :: String -> Maybe ([(String, String)], String)
parseObject s =
    case skipWhitespace s of
        ('{':rest) -> parseFields rest
        _ -> Nothing

parseFields :: String -> Maybe ([(String, String)], String)
parseFields s =
    case skipWhitespace s of
        '}':rest -> Just ([], rest)
        _ -> do
            ((key, value), rest1) <- parseKeyValue s
            case skipWhitespace rest1 of
                ',':rest2 -> do
                    (remaining, rest3) <- parseFields rest2
                    return ((key, value):remaining, rest3)
                '}':rest2 -> Just ([(key, value)], rest2)
                _ -> Nothing

parseKeyValue :: String -> Maybe ((String, String), String)
parseKeyValue s = do
    (key, rest1) <- parseString (skipWhitespace s)
    rest2 <- case skipWhitespace rest1 of
        ':':r -> Just r
        _ -> Nothing
    (value, rest3) <- parseString (skipWhitespace rest2)
    return ((key, value), rest3)

parseHeader :: String -> Maybe (Header, String)
parseHeader s = do
  (fields, rest) <- parseObject s
  title <- lookup "title" fields
  let author = lookup "author" fields
  let date = lookup "date" fields
  return (Header title author date, rest)

parseBody :: String -> Maybe [String]
parseBody s = Just [s] -- Exemple simple, à adapter selon le format réel

parseDocument :: String -> Maybe Document
parseDocument s = do
    (docObj, _) <- parseObject s
    headerStr <- lookup "header" docObj
    (header, _) <- parseHeader headerStr  -- Extraire seulement le Header, pas le reste
    return $ Document header []

