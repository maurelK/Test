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
  let jsonString = "{ \"header\": \"{ \\\"title\\\": \\\"My Document\\\", \\\"author\\\": \\\"John Doe\\\", \\\"date\\\": \\\"2025-05-03\\\" }\" }"

  
  -- Test de la fonction parseDocument
  let result = parseDocument jsonString
  
  -- Affichage du résultat
  case result of
    Just document -> print document
    Nothing -> putStrLn "Erreur lors de l'analyse du document."
