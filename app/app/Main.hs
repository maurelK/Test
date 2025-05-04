{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Main file
-}
import ParserJson(parseDocument)
import MyDocFormat

main :: IO ()
main = do
  content <- readFile "simple.json"
  putStrLn "Tentative de parsing..."
  case parseDocument content of
      Just doc -> do
          putStrLn "Parsing réussi !"
          print doc
      Nothing -> putStrLn "Échec. Essayez ceci :"
          >> print (parseDocument "{ \"header\": {\"title\":\"Test\"}, \"body\": [] }")