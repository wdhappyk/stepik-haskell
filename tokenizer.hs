import Data.Char (isDigit)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x = case x of
    "+" -> Just Plus
    "-" -> Just Minus
    "(" -> Just LeftBrace
    ")" -> Just RightBrace
    x   -> if all isDigit x then Just (Number (read x)) else Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words
