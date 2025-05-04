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

-- Main content par
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