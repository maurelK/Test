{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Parser file
-}
module ParserJson (parseDocument) where

import MyDocFormat

skipWhitespace :: String -> String
skipWhitespace = dropWhile (\c -> c `elem` " \t\n\r")


-- Parse the full document
parseDocument :: String -> Maybe Document
parseDocument s = do
  (fields, _) <- parseFields s
  -- Ne pas traiter header comme une chaîne JSON mais directement comme un objet
  (hdrFields, _) <- lookup "header" fields >>= parseFields
  title <- lookup "title" hdrFields
  let author = lookup "author" hdrFields
  let date = lookup "date" hdrFields
  let hdr = Header title author date
  
  -- Traiter content directement comme un tableau
  contentStr <- lookup "content" fields
  (content, _) <- parseContentArray contentStr
  Just $ Document hdr content

--parseDocument s = do
--  (fields, _) <- parseFields s
--  headerJson <- lookup "header" fields
--  (Header title author date, _) <- parseHeader headerJson  -- <- Premier niveau de parsing
--  
--  contentJson <- lookup "content" fields
--  (content, _) <- parseContentArray contentJson -- # <- Deuxième niveau de parsing
--  
--  return $ Document (Header title author date) content

-- Parse header object
parseHeader :: String -> Maybe (Header, String)
parseHeader s = do
  (fields, rest) <- parseFields s
  title <- lookup "title" fields        -- title is required
  let author = lookup "author" fields   -- optional
  let date = lookup "date" fields       -- optional
  return (Header title author date, rest)



-- Parse a single content object
parseContent :: String -> Maybe (Content, String)
parseContent s = do
  (fields, rest) <- parseFields s
  case fields of
    [("text", val)] -> Just (Text val, rest)
    [("bold", inner)] -> do
      (innerContent, _) <- parseContent inner
      Just (Bold innerContent, rest)
    [("italic", inner)] -> do
      (innerContent, _) <- parseContent inner
      Just (Italic innerContent, rest)
    [("paragraph", arr)] -> do
      (innerArr, _) <- parseContentArray arr
      Just (Paragraph innerArr, rest)
    _ -> Nothing

-- Parse object fields: { "key": "value", ... }

parseFields :: String -> Maybe ([(String, String)], String)
parseFields s = case skipWhitespace s of
  ('{':rest) -> parsePairs (skipWhitespace rest) []
  _ -> Nothing
  where
    parsePairs ('}':xs) acc = Just (reverse acc, skipWhitespace xs)
    parsePairs s acc = do
      (key, afterKey) <- parseString (skipWhitespace s)
      afterColon <- stripPrefix ':' (skipWhitespace afterKey)
      (value, afterValue) <- parseValue (skipWhitespace afterColon)
      let next = skipWhitespace afterValue
      parsePairs next ((key, value) : acc)

parseContentArray :: String -> Maybe ([Content], String)
parseContentArray s = case skipWhitespace s of
  ('[':rest) -> parseElems (skipWhitespace rest) []
  _ -> Nothing
  where
    parseElems (']':xs) acc = Just (reverse acc, skipWhitespace xs)
    parseElems s acc = do
      (c, rest1) <- parseContent (skipWhitespace s)
      let rest2 = skipWhitespace rest1
      parseElems rest2 (c : acc)

-- Parse either a string or a nested object/array as a value
parseValue :: String -> Maybe (String, String)
parseValue s = case skipWhitespace s of
  ('"':_) -> parseString s
  ('{':_) -> wrap parseObject s
  ('[':_) -> wrap parseArray s
  _ -> Nothing
  where
    wrap p str = do
      (val, rest) <- p str
      Just (val, skipWhitespace rest)
  
-- Parse a JSON object as a raw string
parseObject :: String -> Maybe (String, String)
parseObject s = extractBraces '{' '}' s

-- Parse a JSON array as a raw string
parseArray :: String -> Maybe (String, String)
parseArray s = extractBraces '[' ']' s

-- Parse a quoted string: "something"
--parseString :: String -> Maybe (String, String)
--parseString ('"':rest) = go "" rest
--  where
--    go acc ('"':xs) = Just (reverse acc, xs)
--    go acc (x:xs)   = go (x:acc) xs
--    go _ []         = Nothing
--parseString _ = Nothing

parseString :: String -> Maybe (String, String)
parseString ('"':rest) = go "" rest
  where
    go acc ('\\':x:xs) = go (x:acc) xs  -- Handles escaped characters
    go acc ('"':xs) = Just (reverse acc, xs)
    go acc (x:xs) = go (x:acc) xs
    go _ [] = Nothing
parseString _ = Nothing  -- Handle all other cases

-- Strip prefix if it matches
stripPrefix :: Char -> String -> Maybe String
stripPrefix c (x:xs) | x == c = Just xs
stripPrefix _ _ = Nothing

-- Extract content between balanced braces/brackets
extractBraces :: Char -> Char -> String -> Maybe (String, String)
extractBraces open close s@(x:_)
  | x /= open = Nothing
  | otherwise = go 0 "" s
  where
    go :: Int -> String -> String -> Maybe (String, String)
    go _ acc [] = Nothing
    go depth acc (y:ys)
      | y == open = go (depth + 1) (y:acc) ys
      | y == close =
          let newDepth = depth - 1
          in if newDepth == 0
             then Just (reverse (y:acc), ys)
             else go newDepth (y:acc) ys
      | otherwise = go depth (y:acc) ys
extractBraces _ _ _ = Nothing
