module Persons where

import Data.Char (isDigit, digitToInt)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show
data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

str = "firstName = John\nlastName = Connor\nage = 30"

parsePerson :: String -> Either Error Person
parsePerson str
    | not . isCorrectPairs $ pairs' = Left ParsingError
    | isCorrect && isInt age = let
                                 (Just fn) = getVal "firstName" pairs
                                 (Just ln) = getVal "lastName" pairs
                                 (Just a) = strToInt age
                               in Right (Person (fn::String) (ln::String) a)
    | isCorrect && not (isInt age) = Left (IncorrectDataError (age::String))
    | otherwise = Left IncompleteDataError
  where
    pairs' = getPairs ('\n', '=') str
    pairs = getCorrectPairs pairs'
    keys = ["firstName", "lastName", "age"]
    isCorrect = and . map (\k -> hasKey k pairs) $ keys
    (Just age) = getVal "age" pairs



hasKey :: [Char] -> [([Char], a)] -> Bool
hasKey key = or . map (\(k,_) -> key == k)

getVal :: [Char] -> [([Char], a)] -> Maybe a
getVal key pairs | hasKey key pairs = let
                                        [(_,val)] = take 1 $ filter (\(k,_) -> k == key) pairs
                                      in Just val
                 | otherwise        = Nothing

getPairs :: (Char, Char) -> [Char] -> [Maybe ([Char], [Char])]
getPairs (row,eq) = map helper . map (split eq) . split (row) where
  helper [a,b] = Just (trimEnd a, trimStart b)
  helper _     = Nothing

getCorrectPairs :: [Maybe a] -> [a]
getCorrectPairs = map helper where
  helper ~(Just a) = a

isCorrectPairs :: [Maybe a] -> Bool
isCorrectPairs = and . map helper where
  helper (Just _) = True
  helper _        = False

strToInt :: [Char] -> Maybe Int
strToInt val
    | isInt val = Just res
    | otherwise = Nothing
  where
    res = fst $ foldr (\n (r,s) -> (digitToInt n * 10^s + r, s + 1)) (0,0) val

isInt :: [Char] -> Bool
isInt = all isDigit

split :: Char -> [Char] -> [[Char]]
split _ []  = []
split c str = let
        sub = takeWhile (/= c) . drop 0 $ str
        n = drop (length sub + 1) str
    in sub : split c n

trim :: [Char] -> [Char]
trim = trimEnd . trimStart

trimStart :: [Char] -> [Char]
trimStart xs@(x:xs') | x == ' '  = trimStart xs'
                     | otherwise = xs

trimEnd :: [Char] -> [Char]
trimEnd = reverse . trimStart . reverse