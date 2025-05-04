-- EPITECH PROJECT, 2024
-- Markdown Parser for mypandoc project
module ParserMarkdown (parseDocumentMarkdown) where

import MyDocFormat
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)

-- Main entry point for Markdown parsing
parseDocumentMarkdown :: String -> Maybe Document
parseDocumentMarkdown input =
  let ls = lines input
      (headerLines, restLines) = span (/= "---") (dropWhile (/= "---") ls)
      header = parseHeaderMarkdown headerLines
      bodyLines = drop 1 restLines -- skip the '---' line
      body = parseBodyMarkdown bodyLines
  in Just $ Document header body

-- Parse the header block in Markdown (between --- lines)
parseHeaderMarkdown :: [String] -> Header
parseHeaderMarkdown ls =
  let titleLine = findLineWithPrefix "title:" ls
      authorLine = findLineWithPrefix "author:" ls
      dateLine = findLineWithPrefix "date:" ls
      title = fromMaybe "" (stripPrefix "title: " =<< titleLine)
      author = stripPrefix "author: " =<< authorLine
      date = stripPrefix "date: " =<< dateLine
  in Header title author date

-- Helper to find a line starting with a prefix
findLineWithPrefix :: String -> [String] -> Maybe String
findLineWithPrefix prefix = foldr (\l acc -> if prefix `isPrefixOf` l then Just l else acc) Nothing

-- Parse the body lines into Content list
parseBodyMarkdown :: [String] -> [Content]
parseBodyMarkdown [] = []
parseBodyMarkdown (l:ls)
  | null l = parseBodyMarkdown ls
  | isPrefixOf "# " l = Section (SectionData (Just (drop 2 l)) (parseBodyMarkdown ls)) : []
  | isPrefixOf "## " l = Section (SectionData (Just (drop 3 l)) (parseBodyMarkdown ls)) : []
  | isPrefixOf "- " l = parseListMarkdown Unordered (l:ls)
  | isPrefixOf "1. " l = parseListMarkdown Ordered (l:ls)
  | otherwise = Paragraph [Text l] : parseBodyMarkdown ls

-- Parse a list (ordered or unordered)
parseListMarkdown :: ListType -> [String] -> [Content]
parseListMarkdown lt ls =
  let (items, rest) = span (isListItem lt) ls
      listItems = map (ListItem . (:[]) . Text . dropListMarker lt) items
  in List (ListData lt listItems) : parseBodyMarkdown rest

-- Check if a line is a list item of given type
isListItem :: ListType -> String -> Bool
isListItem Unordered l = isPrefixOf "- " l
isListItem Ordered l = case span (/= '.') l of
  (num, '.':rest) -> all (`elem` ['0'..'9']) num
  _ -> False

-- Drop the list marker from a list item line
dropListMarker :: ListType -> String -> String
dropListMarker Unordered l = drop 2 l
dropListMarker Ordered l = case span (/= '.') l of
  (_, '.':rest) -> dropWhile (== ' ') rest
  _ -> l
