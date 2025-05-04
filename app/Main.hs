{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Main file
-}
import ParserJson
import MyDocFormat

main :: IO ()
main = do
  let input = "{ \"header\": \"{\\\"title\\\": \\\"Doc Title\\\"}\", \"content\": \"[{\\\"text\\\":\\\"Hello\\\"},{\\\"bold\\\":{\\\"text\\\":\\\"world\\\"}}]\" }"
  print $ parseDocument input
