helloPerson :: String -> String
helloPerson name = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  putStrLn (helloPerson name)
