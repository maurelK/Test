module Main (main) where

import Distance
import Usage

main :: IO ()
main = do
    args <- getArgs
    if length args /= 6 || args !! 0 /= "-n" || args !! 2 /= "-l" || args !! 4 /= "-f"
        then print(lol)
    else do
        let k = read $ args !! 1
            limit = read $ args !! 3
            filePath = args !! 5
        content <- readFile filePath
        let pixels = parseInput content
            clusters = kmeans pixels k limit
        putStrLn $ formatOutput clusters