import Data.List (isInfixOf)

main' :: IO ()
main' = do
    putStr "Substring: "
    str <- getLine
    if null str
    then putStrLn "Canceled"
    else rm str
    
rm :: String -> IO ()
rm str = do
    files <- getDirectoryContents "."
    let filesToRemove = filter (isInfixOf str) files
    -- mapM_ removeFile filesToRemove
    mapM_ (\f -> putStrLn ("Removing file: " ++ f)) filesToRemove

