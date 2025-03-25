module Usage(usage) where

usage :: IO ()
usage = do
    putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n\n"
    putStrLn "\tN\tnumber of colors in the final image\n"
    putStrLn "\tL\tconvergence limit\n"
    putStrLn "\tF\tpath to the file containing the colors of the pixels"