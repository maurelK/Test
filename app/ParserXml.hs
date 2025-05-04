{-
-- EPITECH PROJECT, 2024
-- XML Parser for mypandoc project
-}
module ParserXml (parseDocumentXml) where

import MyDocFormat
import Data.Char (isSpace)
import Data.List (stripPrefix)
import Debug.Trace (trace)

-- Parse the entire XML document string into Document
parseDocumentXml :: String -> Maybe Document
parseDocumentXml s = do
    let s' = skipWhitespace s
    (tag, attrs, inner, rest) <- parseTag s'
    if tag /= "document"
      then Nothing
      else do
        (header, restHeader) <- parseHeader inner
        (body, restBody) <- parseBodyXml restHeader
        if not (null restBody)
          then Nothing
          else Just $ Document header body

-- Parse header element
parseHeader :: String -> Maybe (Header, String)
parseHeader s = do
    let s' = skipWhitespace s
    (tag, attrs, inner, rest) <- parseTag s'
    if tag /= "header"
      then Nothing
      else do
        let mtitle = lookup "title" attrs
        let mauthor = lookup "author" attrs
        let mdate = lookup "date" attrs
        let title = case mtitle of
                      Just t -> t
                      Nothing -> ""
        return (Header title mauthor mdate, rest)

-- Parse body element and its contents
parseBodyXml :: String -> Maybe ([Content], String)
parseBodyXml s = do
    let s' = skipWhitespace s
    (tag, attrs, inner, rest) <- parseTag s'
    if tag /= "body"
      then Nothing
      else do
        contents <- parseContentsXml inner
        return (contents, rest)

-- Parse multiple content elements inside body or paragraph
parseContentsXml :: String -> Maybe [Content]
parseContentsXml s = parseContentsHelper s []

parseContentsHelper :: String -> [Content] -> Maybe [Content]
parseContentsHelper s acc
  | null (skipWhitespace s) = Just (reverse acc)
  | otherwise = do
      (tag, attrs, inner, rest) <- parseTag (skipWhitespace s)
      content <- parseContentXml tag attrs inner
      parseContentsHelper rest (content : acc)

-- Parse a single content element based on tag
parseContentXml :: String -> [(String,String)] -> String -> Maybe Content
parseContentXml "text" _ inner = Just $ Text inner
parseContentXml "bold" _ inner = do
    contents <- parseContentsXml inner
    Just $ Bold (combineContents contents)
parseContentXml "italic" _ inner = do
    contents <- parseContentsXml inner
    Just $ Italic (combineContents contents)
parseContentXml "code" _ inner = do
    contents <- parseContentsXml inner
    Just $ Code (combineContents contents)
parseContentXml "paragraph" _ inner = do
    contents <- parseContentsXml inner
    Just $ Paragraph contents
parseContentXml "section" attrs inner = do
    let mtitle = lookup "title" attrs
    contents <- parseContentsXml inner
    Just $ Section (SectionData mtitle contents)
parseContentXml "codeblock" _ inner = Just $ CodeBlock inner
parseContentXml "list" attrs inner = do
    items <- parseListItemsXml inner
    let listType = case lookup "type" attrs of
                      Just "ordered" -> Ordered
                      _ -> Unordered
    Just $ List (ListData listType items)
parseContentXml "link" attrs inner = do
    url <- lookup "url" attrs
    contents <- parseContentsXml inner
    Just $ Link (LinkData contents url)
parseContentXml "image" attrs _ = do
    url <- lookup "url" attrs
    alt <- lookup "alt" attrs
    Just $ Image (ImageData alt url)
parseContentXml _ _ _ = Nothing

-- Combine multiple contents into one if possible, else return first
combineContents :: [Content] -> Content
combineContents [c] = c
combineContents cs = Paragraph cs

-- Parse list items inside a list
parseListItemsXml :: String -> Maybe [ListItem]
parseListItemsXml s = parseListItemsHelper s []

parseListItemsHelper :: String -> [ListItem] -> Maybe [ListItem]
parseListItemsHelper s acc
  | null (skipWhitespace s) = Just (reverse acc)
  | otherwise = do
      (tag, attrs, inner, rest) <- parseTag (skipWhitespace s)
      if tag /= "item"
        then Nothing
        else do
          contents <- parseContentsXml inner
          parseListItemsHelper rest (ListItem contents : acc)

-- Parse a single XML tag with attributes and inner content
parseTag :: String -> Maybe (String, [(String,String)], String, String)
parseTag s = do
    s1 <- stripPrefix "<" s
    let (tagName, rest1) = span (\c -> not (isSpace c) && c /= '>' && c /= '/') s1
    let (attrs, rest2) = parseAttributes rest1 []
    case rest2 of
      ('/':'>':rest) -> Just (tagName, attrs, "", rest)
      ('>':rest) -> do
        (inner, rest3) <- parseUntilCloseTag tagName rest
        Just (tagName, attrs, inner, rest3)
      _ -> Nothing

-- Parse attributes inside a tag
parseAttributes :: String -> [(String,String)] -> ([(String,String)], String)
parseAttributes s acc = case skipWhitespace s of
    ('>':rest) -> (reverse acc, '>' : rest)
    ('/':'>':rest) -> (reverse acc, '/' : '>' : rest)
    _ -> case span (/= '=') s of
        (key, '=':rest1) -> case parseAttributeValue rest1 of
            Just (val, rest2) -> parseAttributes rest2 ((key, val) : acc)
            _ -> (reverse acc, s)
        _ -> (reverse acc, s)

-- Parse attribute value in quotes
parseAttributeValue :: String -> Maybe (String, String)
parseAttributeValue ('"':s) = go "" s
  where
    go acc ('"':rest) = Just (reverse acc, rest)
    go acc (c:cs) = go (c:acc) cs
    go _ [] = Nothing
parseAttributeValue _ = Nothing

-- Parse content until closing tag
parseUntilCloseTag :: String -> String -> Maybe (String, String)
parseUntilCloseTag tag s = go 0 "" s
  where
    openTag = "<" ++ tag
    closeTag = "</" ++ tag ++ ">"
    go :: Int -> String -> String -> Maybe (String, String)
    go 0 acc [] = Nothing
    go 0 acc str@(c:cs)
      | closeTag `isPrefixOf` str = Just (reverse acc, drop (length closeTag) str)
      | openTag `isPrefixOf` str = go (1) (c:acc) cs
      | otherwise = go 0 (c:acc) cs
    go n acc [] = Nothing
    go n acc str@(c:cs)
      | closeTag `isPrefixOf` str = go (n-1) (c:acc) (drop (length closeTag) str)
      | openTag `isPrefixOf` str = go (n+1) (c:acc) (drop (length openTag) str)
      | otherwise = go n (c:acc) cs

-- Skip whitespace characters
skipWhitespace :: String -> String
skipWhitespace = dropWhile isSpace

-- Check if a string is prefix of another
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
