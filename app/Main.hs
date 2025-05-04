{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Main file
-}
import ParserJson (parseDocument)
import ParserXml (parseDocumentXml)
import ParserMarkdown (parseDocumentMarkdown)
import MyDocFormat
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf)

data Format = XML | JSON | Markdown deriving (Eq, Show)

-- Detect format from file extension
detectFormat :: String -> Maybe Format
detectFormat filename
  | ".xml" `isSuffixOf` filename = Just XML
  | ".json" `isSuffixOf` filename = Just JSON
  | ".md" `isSuffixOf` filename = Just Markdown
  | otherwise = Nothing

-- Parse input based on format
parseInput :: Format -> String -> Maybe Document
parseInput JSON = parseDocument
parseInput XML = parseDocumentXml
parseInput Markdown = parseDocumentMarkdown

-- Output formatting functions
formatOutput :: Format -> Document -> String
formatOutput XML doc = formatXml doc
formatOutput JSON doc = formatJson doc
formatOutput Markdown doc = formatMarkdown doc

-- Format Document to XML string
formatXml :: Document -> String
formatXml doc = 
  "<document>\n" ++
  formatHeaderXml (docHeader doc) ++
  formatBodyXml (docBody doc) ++
  "</document>\n"

formatHeaderXml :: Header -> String
formatHeaderXml (Header t a d) =
  "  <header title=\"" ++ t ++ "\"" ++
  maybe "" (\x -> " author=\"" ++ x ++ "\"") a ++
  maybe "" (\x -> " date=\"" ++ x ++ "\"") d ++
  "></header>\n"

formatBodyXml :: [Content] -> String
formatBodyXml contents =
  "  <body>\n" ++ concatMap formatContentXml contents ++ "  </body>\n"

formatContentXml :: Content -> String
formatContentXml (Text s) = "    <text>" ++ s ++ "</text>\n"
formatContentXml (Bold c) = "    <bold>" ++ formatContentXml c ++ "    </bold>\n"
formatContentXml (Italic c) = "    <italic>" ++ formatContentXml c ++ "    </italic>\n"
formatContentXml (Code c) = "    <code>" ++ formatContentXml c ++ "    </code>\n"
formatContentXml (Paragraph cs) = "    <paragraph>\n" ++ concatMap formatContentXml cs ++ "    </paragraph>\n"
formatContentXml (Section (SectionData mt cs)) =
  "    <section" ++ maybe "" (\t -> " title=\"" ++ t ++ "\"") mt ++ ">\n" ++
  concatMap formatContentXml cs ++
  "    </section>\n"
formatContentXml (CodeBlock s) = "    <codeblock>" ++ s ++ "</codeblock>\n"
formatContentXml (List (ListData lt items)) =
  "    <list type=\"" ++ (if lt == Ordered then "ordered" else "unordered") ++ "\">\n" ++
  concatMap formatListItemXml items ++
  "    </list>\n"
formatContentXml (Link (LinkData cs url)) =
  "    <link url=\"" ++ url ++ "\">\n" ++ concatMap formatContentXml cs ++ "    </link>\n"
formatContentXml (Image (ImageData alt url)) =
  "    <image alt=\"" ++ alt ++ "\" url=\"" ++ url ++ "\"/>\n"

formatListItemXml :: ListItem -> String
formatListItemXml (ListItem cs) =
  "      <item>\n" ++ concatMap formatContentXml cs ++ "      </item>\n"

-- Format Document to JSON string manually (no external libs)
formatJson :: Document -> String
formatJson doc =
  "{\n" ++
  "  \"header\": " ++ headerToJson (docHeader doc) ++ ",\n" ++
  "  \"body\": [" ++ bodyToJson (docBody doc) ++ "]\n" ++
  "}\n"

headerToJson :: Header -> String
headerToJson (Header t a d) =
  "{ " ++
  "\"title\": " ++ show t ++
  maybe "" (\x -> ", \"author\": " ++ show x) a ++
  maybe "" (\x -> ", \"date\": " ++ show x) d ++
  " }"

bodyToJson :: [Content] -> String
bodyToJson cs = concat $ intersperse ", " (map contentToJson cs)

contentToJson :: Content -> String
contentToJson (Text s) = "{ \"text\": " ++ show s ++ " }"
contentToJson (Bold c) = "{ \"bold\": " ++ contentToJson c ++ " }"
contentToJson (Italic c) = "{ \"italic\": " ++ contentToJson c ++ " }"
contentToJson (Code c) = "{ \"code\": " ++ contentToJson c ++ " }"
contentToJson (Paragraph cs) = "{ \"paragraph\": [" ++ bodyToJson cs ++ "] }"
contentToJson (Section (SectionData mt cs)) =
  "{ \"section\": { " ++
  maybe "" (\t -> "\"title\": " ++ show t ++ ", ") mt ++
  "\"content\": [" ++ bodyToJson cs ++ "] } }"
contentToJson (CodeBlock s) = "{ \"codeblock\": " ++ show s ++ " }"
contentToJson (List (ListData lt items)) =
  "{ \"list\": [" ++ listItemsToJson items ++ "], \"type\": " ++ showListType lt ++ " }"
contentToJson (Link (LinkData cs url)) =
  "{ \"link\": { \"url\": " ++ show url ++ ", \"content\": [" ++ bodyToJson cs ++ "] } }"
contentToJson (Image (ImageData alt url)) =
  "{ \"image\": { \"alt\": " ++ show alt ++ ", \"url\": " ++ show url ++ " } }"

listItemsToJson :: [ListItem] -> String
listItemsToJson items = concat $ intersperse ", " (map listItemToJson items)

listItemToJson :: ListItem -> String
listItemToJson (ListItem cs) = "{ \"item\": [" ++ bodyToJson cs ++ "] }"

showListType :: ListType -> String
showListType Ordered = "\"ordered\""
showListType Unordered = "\"unordered\""

-- Helper function: intersperse
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

-- Format Document to Markdown string
formatMarkdown :: Document -> String
formatMarkdown (Document header body) =
  "---\n" ++
  "title: " ++ title header ++ "\n" ++
  maybe "" (\a -> "author: " ++ a ++ "\n") (author header) ++
  maybe "" (\d -> "date: " ++ d ++ "\n") (date header) ++
  "---\n\n" ++
  concatMap formatContentMarkdown body

formatContentMarkdown :: Content -> String
formatContentMarkdown (Text s) = s ++ "\n\n"
formatContentMarkdown (Bold c) = "**" ++ extractText c ++ "**"
formatContentMarkdown (Italic c) = "*" ++ extractText c ++ "*"
formatContentMarkdown (Code c) = "`" ++ extractText c ++ "`"
formatContentMarkdown (Paragraph cs) = concatMap formatContentMarkdown cs ++ "\n"
formatContentMarkdown (Section (SectionData mt cs)) =
  maybe "" (\t -> "# " ++ t ++ "\n\n") mt ++ concatMap formatContentMarkdown cs
formatContentMarkdown (CodeBlock s) = "```\n" ++ s ++ "\n```\n"
formatContentMarkdown (List (ListData lt items)) =
  concatMap (formatListItemMarkdown lt) (zip [1..] items)
formatContentMarkdown (Link (LinkData cs url)) =
  "[" ++ concatMap extractText cs ++ "](" ++ url ++ ")"
formatContentMarkdown (Image (ImageData alt url)) =
  "![" ++ alt ++ "](" ++ url ++ ")"

formatListItemMarkdown :: ListType -> (Int, ListItem) -> String
formatListItemMarkdown Ordered (i, ListItem cs) =
  show i ++ ". " ++ concatMap extractText cs ++ "\n"
formatListItemMarkdown Unordered (_, ListItem cs) =
  "- " ++ concatMap extractText cs ++ "\n"

extractText :: Content -> String
extractText (Text s) = s
extractText (Bold c) = extractText c
extractText (Italic c) = extractText c
extractText (Code c) = extractText c
extractText (Paragraph cs) = concatMap extractText cs
extractText (Section (SectionData _ cs)) = concatMap extractText cs
extractText (CodeBlock s) = s
extractText (List (ListData _ items)) = concatMap extractTextListItem items
extractText (Link (LinkData cs _)) = concatMap extractText cs
extractText (Image (ImageData alt _)) = alt

extractTextListItem :: ListItem -> String
extractTextListItem (ListItem cs) = concatMap extractText cs


data Options = Options
  { inputFile :: Maybe String
  , outputFile :: Maybe String
  , inputFormat :: Maybe Format
  , outputFormat :: Maybe Format
  }

parseFormat :: String -> Format
parseFormat "xml" = XML
parseFormat "json" = JSON
parseFormat "markdown" = Markdown
parseFormat _ = Markdown -- default fallback

parseArgs :: [String] -> Options -> Maybe Options
parseArgs [] opts = Just opts
parseArgs ("-i":f:rest) opts = parseArgs rest (opts { inputFile = Just f })
parseArgs ("-f":f:rest) opts = parseArgs rest (opts { outputFormat = Just (parseFormat f) })
parseArgs ("-o":f:rest) opts = parseArgs rest (opts { outputFile = Just f })
parseArgs ("-e":f:rest) opts = parseArgs rest (opts { inputFormat = Just (parseFormat f) })
parseArgs (_:rest) opts = Nothing

main :: IO ()
main = do
  args <- getArgs
  let usage = "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]"
  case parseArgs args (Options Nothing Nothing Nothing Nothing) of
    Nothing -> do
      hPutStrLn stderr usage
      exitFailure
    Just opts -> do
      case inputFile opts of
        Nothing -> do
          hPutStrLn stderr usage
          exitFailure
        Just infile -> do
          content <- readFile infile
          let detectedFmt = detectFormat infile
          let infmt = case inputFormat opts of
                        Just f -> if Just f == detectedFmt
                                  then Just f
                                  else detectedFmt
                        Nothing -> detectedFmt
          case infmt of
            Nothing -> do
              hPutStrLn stderr "Cannot detect input format"
              exitFailure
            Just fmt -> do
              let mdoc = parseInput fmt content
              case mdoc of
                Nothing -> do
                  hPutStrLn stderr "Failed to parse input file"
                  exitFailure
                Just doc -> do
                  let outfmt = fromMaybe JSON (outputFormat opts)
                  let output = formatOutput outfmt doc
                  case outputFile opts of
                    Nothing -> putStrLn output
                    Just outfile -> writeFile outfile output
