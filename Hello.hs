main' :: IO ()
main' = do
  name <- getName  
  putStrLn $ "Hi, " ++ name ++ "!"

getName :: IO String
getName = do
  putStr "What is your name?\nName: "
  name <- getLine
  if name == "" then getName else return name




