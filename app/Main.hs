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
  content <- readFile "syntaxe.json"
  case parseDocument content of
      Just doc -> print doc
      Nothing -> putStrLn "Échec du parsing : le JSON est invalide ou mal formé."
