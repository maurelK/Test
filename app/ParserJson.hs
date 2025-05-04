{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Parser file
-}
module ParserJson (parseJsonDoc) where

import MyDocFormat

-- Parse the full document
--parseDocument :: String -> Maybe Document
--parseDocument s = do
--  (fields, _) <- parseFields s
--  headerStr <- lookup "header" fields
--  (hdr, _) <- parseHeader headerStr
--  contentStr <- lookup "content" fields
--  (content, _) <- parseContentArray contentStr
--  Just $ Document hdr content

--toJsonDoc :: Document -> String
--toXmlDoc :: Document -> String
--toMarkdownDoc :: Document -> String

--parseJsonDoc s = do
--  (fields, _) <- parseFields s
--  headerJson <- lookup "header" fields
--  (Header title author date, _) <- parseHeader headerJson  -- <- Premier niveau de parsing
--  
--  contentJson <- lookup "content" fields
--  (content, _) <- parseContentArray contentJson -- # <- Deuxième niveau de parsing
--  
--  return $ Document (Header title author date) content



parseJsonDoc :: String -> Either String Document
parseJsonDoc s = case parseFields s of
  Nothing -> Left "Invalid JSON object"
  Just (fields, _) -> do
    case lookup "header" fields of
      Nothing -> Left "Missing 'header' field"
      Just headerJson -> case parseHeader headerJson of
        Nothing -> Left "Failed to parse header"
        Just (Header title author date, _) -> case lookup "content" fields of
          Nothing -> Left "Missing 'content' field"
          Just contentJson -> case parseContentArray contentJson of
            Nothing -> Left "Failed to parse content"
            Just (content, _) -> Right $ Document (Header title author date) content

-- Parse header object
parseHeader :: String -> Maybe (Header, String)
parseHeader s = do
  (fields, rest) <- parseFields s
  title <- lookup "title" fields        -- title is required
  let author = lookup "author" fields   -- optional
  let date = lookup "date" fields       -- optional
  return (Header title author date, rest)


-- Parse content array
parseContentArray :: String -> Maybe ([Content], String)
parseContentArray ('[':rest) = parseElems rest []
  where
    parseElems (']':xs) acc = Just (reverse acc, xs)
    parseElems s acc = do
      (c, rest1) <- parseContent s
      let rest2 = dropWhile (`elem` ", \n") rest1
      parseElems rest2 (c : acc)
parseContentArray _ = Nothing

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
parseFields ('{':rest) = parsePairs rest []
  where
    parsePairs ('}':xs) acc = Just (reverse acc, xs)
    parsePairs s acc = do
      (key, afterKey) <- parseString s
      afterColon <- stripPrefix ':' afterKey
      (value, afterValue) <- parseValue afterColon
      let next = dropWhile (`elem` ", \n") afterValue
      parsePairs next ((key, value) : acc)
parseFields _ = Nothing

-- Parse either a string or a nested object/array as a value
parseValue :: String -> Maybe (String, String)
parseValue s@(x:_)
  | x == '"'  = parseString s
  | x == '{'  = wrap parseObject s
  | x == '['  = wrap parseArray s
  | otherwise = Nothing
  where
    wrap p str = do
      (val, rest) <- p str
      Just (val, rest)

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

parseString ('"':rest) = go "" rest
  where
    go acc ('\\':x:xs) = go (x:acc) xs  -- Gère les caractères échappés
    go acc ('"':xs) = Just (reverse acc, xs)
    go acc (x:xs) = go (x:acc) xs
    go _ [] = Nothing

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
