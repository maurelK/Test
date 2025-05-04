{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Parser file
-}
module ParserJson (parseDocument) where

import MyDocFormat
import Data.Char (isDigit)
import Data.List (find)
import Debug.Trace (trace)

-- Main document parser
parseDocument :: String -> Maybe Document
parseDocument s = do
    trace ("parseDocument input: " ++ take 100 s) $ return ()
    (fields, rest) <- parseFields s
    trace ("parseFields result: " ++ show fields ++ ", rest: " ++ take 50 rest) $ return ()
    if not (null (skipWhitespace rest))
      then trace "Extra data after parsing fields" Nothing
      else do
        (headerFields, restHeader) <- lookup "header" fields >>= parseFields
        trace ("headerFields: " ++ show headerFields ++ ", restHeader: " ++ take 50 restHeader) $ return ()
        if not (null (skipWhitespace restHeader))
          then trace "Extra data after parsing header fields" Nothing
          else do
            title <- lookup "title" headerFields
            let author = lookup "author" headerFields
            let date = lookup "date" headerFields
            bodyContent <- lookup "body" fields
            trace ("bodyContent: " ++ take 100 bodyContent) $ return ()
            (contents, restBody) <- parseBody bodyContent
            trace ("parseBody result: " ++ show (map show contents) ++ ", restBody: " ++ take 50 restBody) $ return ()
            if not (null (skipWhitespace restBody))
              then trace "Extra data after parsing body content" Nothing
              else Just $ Document (Header title author date) contents

-- Parse the body content which can be arrays or objects
parseBody :: String -> Maybe ([Content], String)
parseBody s = trace ("parseBody input: " ++ take 100 s) $ case skipWhitespace s of
    '[':rest -> parseArrayOfContents rest
    '{':rest -> do
        (content, rest') <- parseContent s
        return ([content], rest')
    _ -> Nothing
  where
    parseArrayOfContents s' = do
        (contents, rest) <- parseMixedArray s'
        trace ("parseMixedArray result: " ++ show (map show (concat contents)) ++ ", rest: " ++ take 50 rest) $ return ()
        return (concat contents, rest)

-- Parse mixed array that can contain both strings and objects
parseMixedArray :: String -> Maybe ([[Content]], String)
parseMixedArray s = trace ("parseMixedArray input: " ++ take 100 s) $ case skipWhitespace s of
    ']':rest -> Just ([], rest)
    _ -> do
        (first, rest1) <- parseArrayElement s
        (restElems, rest2) <- parseMixedArrayTail rest1
        Just (first : restElems, rest2)

parseMixedArrayTail :: String -> Maybe ([[Content]], String)
parseMixedArrayTail s = trace ("parseMixedArrayTail input: " ++ take 100 s) $ case skipWhitespace s of
    ',':rest -> parseMixedArray rest
    ']':rest -> Just ([], rest)
    _ -> Nothing

parseArrayElement :: String -> Maybe ([Content], String)
parseArrayElement s = trace ("parseArrayElement input: " ++ take 100 s) $ case skipWhitespace s of
    '[':rest -> do
        (items, rest') <- parseMixedArray rest
        case items of
            [[Text single]] -> return ([Text single], rest')
            _ -> return (concat items, rest')
    '{':rest -> do
        (content, rest') <- parseContent s
        return ([content], rest')
    '"':rest -> do
        (str, rest') <- parseString s
        return ([Text str], rest')
    _ -> Nothing

-- Main content parser
parseContent :: String -> Maybe (Content, String)
parseContent s = do
    (fields, rest) <- parseFields s
    case find (\(k,_) -> k `elem` contentTypes) fields of
        Just ("text", val) -> Just (Text val, rest)
        Just ("bold", inner) -> do
            (content, _) <- parseContent inner
            Just (Bold content, rest)
        Just ("italic", inner) -> do
            (content, _) <- parseContent inner
            Just (Italic content, rest)
        Just ("code", inner) -> do
            (content, _) <- parseContent inner
            Just (Code content, rest)
        Just ("paragraph", arr) -> do
            (contents, _) <- parseBody arr
            Just (Paragraph contents, rest)
        Just ("section", obj) -> do
            (fields, _) <- parseFields obj
            let mtitle = lookup "title" fields  -- mtitle is Maybe String
            (contents, _) <- lookup "content" fields >>= parseBody
            Just (Section (SectionData mtitle contents), rest)
        Just ("codeblock", arr) -> do
            (contents, _) <- parseBody arr
            case contents of
                [Text code] -> Just (CodeBlock code, rest)
                _ -> Nothing
        Just ("list", arr) -> do
            (items, rest') <- parseListItems arr
            Just (List (ListData Unordered items), rest')
        Just ("link", obj) -> do
            (fields, _) <- parseFields obj
            url <- lookup "url" fields
            (contents, _) <- lookup "content" fields >>= parseBody
            Just (Link (LinkData contents url), rest)
        Just ("image", obj) -> do
            (fields, _) <- parseFields obj
            url <- lookup "url" fields
            (altText, _) <- lookup "alt" fields >>= parseString
            Just (Image (ImageData altText url), rest)
        _ -> Nothing
  where
    contentTypes = ["text", "bold", "italic", "code", "paragraph", 
                   "section", "codeblock", "list", "link", "image"]

parseListItems :: String -> Maybe ([ListItem], String)
parseListItems s = do
    (contentsList, rest) <- parseBody s
    return (map (\content -> ListItem [content]) contentsList, rest)

-- Helper functions (same as before but with improved whitespace handling)
skipWhitespace :: String -> String
skipWhitespace = dropWhile (\c -> c `elem` " \t\n\r")

parseFields :: String -> Maybe ([(String, String)], String)
parseFields s = case skipWhitespace s of
    '{':rest -> parsePairs (skipWhitespace rest) []
    _ -> Nothing
  where
    parsePairs ('}':xs) acc = Just (reverse acc, skipWhitespace xs)
    parsePairs s' acc = do
        trace ("parsePairs input: " ++ take 100 s') $ return ()
        (key, afterKey) <- parseString (skipWhitespace s')
        trace ("parsePairs key: " ++ key ++ ", afterKey: " ++ take 50 afterKey) $ return ()
        afterColon <- stripPrefix ':' (skipWhitespace afterKey)
        trace ("parsePairs afterColon: " ++ take 50 afterColon) $ return ()
        (value, afterValue) <- parseValue afterColon
        trace ("parsePairs value: " ++ take 100 value ++ ", afterValue: " ++ take 50 afterValue) $ return ()
        let next = case skipWhitespace afterValue of
                    ',':xs' -> skipWhitespace xs'
                    xs' -> xs'
        trace ("parsePairs next: " ++ take 100 next) $ return ()
        parsePairs next ((key, value) : acc)

parseValue :: String -> Maybe (String, String)
parseValue s = case skipWhitespace s of
    '"':_ -> parseString s
    '{':_ -> do
        let (objStr, rest) = extractObject s
        return (objStr, rest)
    '[':_ -> do
        let (arrStr, rest) = extractArray s
        return (arrStr, rest)
    _ -> parseLiteral s

-- Helper function to extract the raw substring of an object including braces
extractObject :: String -> (String, String)
extractObject s = go 1 "" (tail s)
  where
    go :: Int -> String -> String -> (String, String)
    go _ acc [] = (reverse acc, [])
    go 0 acc (x:xs) = (reverse (x:acc), xs)
    go n acc (x:xs)
      | x == '{' = go (n + 1) (x:acc) xs
      | x == '}' = go (n - 1) (x:acc) xs
      | otherwise = go n (x:acc) xs

-- Helper function to extract the raw substring of an array including brackets
extractArray :: String -> (String, String)
extractArray s = go 1 "" (tail s)
  where
    go :: Int -> String -> String -> (String, String)
    go _ acc [] = (reverse acc, [])
    go 0 acc (x:xs) = (reverse (x:acc), xs)
    go n acc (x:xs)
      | x == '[' = go (n + 1) (x:acc) xs
      | x == ']' = go (n - 1) (x:acc) xs
      | otherwise = go n (x:acc) xs

parseLiteral :: String -> Maybe (String, String)
parseLiteral s = case span (\c -> not (c `elem` " \t\n\r,}]")) s of
    ("", _) -> Nothing
    (lit, rest) -> Just (lit, skipWhitespace rest)

parseString :: String -> Maybe (String, String)
parseString ('"':rest) = go "" rest
  where
    go acc ('\\':x:xs) = case x of
        '"' -> go ('"':acc) xs
        '\\' -> go ('\\':acc) xs
        _ -> go (x:acc) xs
    go acc ('"':xs) = Just (reverse acc, skipWhitespace xs)
    go acc (x:xs) = go (x:acc) xs
    go _ [] = Nothing
parseString _ = Nothing

stripPrefix :: Char -> String -> Maybe String
stripPrefix c s = case skipWhitespace s of
    (x:xs) | x == c -> Just (skipWhitespace xs)
    _ -> Nothing
