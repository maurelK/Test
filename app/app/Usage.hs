{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- Usage file
-}

module Usage(usage) where

usage :: IO ()
usage = putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\n" >>
        putStrLn "\tN\tifile path to the file to convert" >>
        putStrLn "\tL\toformat output format (xml, json, markdown)" >>
        putStrLn "\tF\tofile path to the output file" >>
        putStrLn "\tF\tiformat input format (xml, json, markdown)"