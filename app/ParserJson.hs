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

-- Main document parser
parseDocument :: String -> Maybe Document
parseDocument s = do
    (fields, _) <- parseFields s
    (headerFields, _) <- lookup "header" fields >>= parseFields
    title <- lookup "title" headerFields
    let author = lookup "author" headerFields
    let date = lookup "date" headerFields
    bodyContent <- lookup "body" fields
    (contents, _) <- parseBody bodyContent
    Just $ Document (Header title author date) contents

-- Parse the body content which can be arrays or objects
parseBody :: String -> Maybe ([Content], String)
parseBody s = case skipWhitespace s of
    '[':rest -> parseArrayOfContents rest
    '{':rest -> do
        (content, rest') <- parseContent s
        return ([content], rest')
    _ -> Nothing
  where
    parseArrayOfContents s' = do
        (contents, rest) <- parseMixedArray s'
        return (concat contents, rest)

-- Parse mixed array that can contain both strings and objects
parseMixedArray :: String -> Maybe ([[Content]], String)
parseMixedArray s = case skipWhitespace s of
    ']':rest -> Just ([], rest)
    _ -> do
        (first, rest1) <- parseArrayElement s
        (restElems, rest2) <- parseMixedArrayTail rest1
        Just (first : restElems, rest2)

parseMixedArrayTail :: String -> Maybe ([[Content]], String)
parseMixedArrayTail s = case skipWhitespace s of
    ',':rest -> parseMixedArray rest
    ']':rest -> Just ([], rest)
    _ -> Nothing

parseArrayElement :: String -> Maybe ([Content], String)
parseArrayElement s = case skipWhitespace s of
    '[':rest -> do  -- Nested array case
        (items, rest') <- parseMixedArray rest
        return (concat items, rest')
    '{':rest -> do  -- Object case
        (content, rest') <- parseContent s
        return ([content], rest')
    '"':rest -> do  -- String case
        (str, rest') <- parseString s
        return ([Text str], rest')
    _ -> Nothing

-- Main content parser
parseContent :: String -> Maybe (Content, String)
parseContent s = do
    (fields, rest) <- parseFields s
    case find (\(k,_) -> k `elem` ["text", "bold", "italic", "paragraph", "section", 
                                  "codeblock", "list", "link", "image"]) fields of
        Just ("text", val) -> Just (Text val, rest)
        Just ("bold", inner) -> do
            (content, _) <- parseContent inner
            Just (Bold content, rest)
        Just ("italic", inner) -> do
            (content, _) <- parseContent inner
            Just (Italic content, rest)
        Just ("paragraph", arr) -> do
            (contents, _) <- parseBody arr
            Just (Paragraph contents, rest)
        Just ("section", obj) -> do
            (fields, _) <- parseFields obj
            title <- lookup "title" fields
            (contents, _) <- lookup "content" fields >>= parseBody
            Just (Section title contents, rest)
        Just ("codeblock", arr) -> do
            (contents, _) <- parseBody arr
            case contents of
                [Text code] -> Just (CodeBlock code, rest)
                _ -> Nothing
        Just ("list", arr) -> do
            (items, _) <- parseBody arr
            Just (Paragraph items, rest)  -- Representing list as Paragraph for simplicity
        Just ("link", obj) -> do
            (fields, _) <- parseFields obj
            url <- lookup "url" fields
            (contents, _) <- lookup "content" fields >>= parseBody
            Just (Paragraph [Text $ "<" ++ url ++ ">"], rest)  -- Simplified link representation
        Just ("image", obj) -> do
            (fields, _) <- parseFields obj
            url <- lookup "url" fields
            alt <- lookup "alt" fields >>= parseString
            Just (Paragraph [Text $ "![" ++ snd alt ++ "](" ++ url ++ ")"], rest)  -- Markdown-style image
        _ -> Nothing

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
        (key, afterKey) <- parseString (skipWhitespace s')
        afterColon <- stripPrefix ':' (skipWhitespace afterKey)
        (value, afterValue) <- parseValue (skipWhitespace afterColon)
        let next = case skipWhitespace afterValue of
                    ',':xs' -> skipWhitespace xs'
                    xs' -> xs'
        parsePairs next ((key, value) : acc)

parseValue :: String -> Maybe (String, String)
parseValue s = case skipWhitespace s of
    '"':_ -> parseString s
    '{':_ -> wrap parseFields s
    '[':_ -> wrap parseMixedArray s
    _ -> parseLiteral s
  where
    wrap p str = do
        (val, rest) <- p str
        Just (show val, skipWhitespace rest)  -- Convert back to string for consistency

parseLiteral :: String -> Maybe (String, String)
parseLiteral s = case span (\c -> not (c `elem` " \t\n\r,}]")) s of
    ("", _) -> Nothing
    (lit, rest) -> Just (lit, skipWhitespace rest)

parseString :: String -> Maybe (String, String)
parseString ('"':rest) = go "" rest
  where
    go acc ('\\':x:xs) = case x of
        'n' -> go ('\n':acc) xs
        't' -> go ('\t':acc) xs
        'r' -> go ('\r':acc) xs
        _ -> go (x:acc) xs
    go acc ('"':xs) = Just (reverse acc, skipWhitespace xs)
    go acc (x:xs) = go (x:acc) xs
    go _ [] = Nothing
parseString _ = Nothing

stripPrefix :: Char -> String -> Maybe String
stripPrefix c s = case skipWhitespace s of
    (x:xs) | x == c -> Just (skipWhitespace xs)
    _ -> Nothing