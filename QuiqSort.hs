module QuiqSort where

quiqSort :: [Int] -> [Int]
quiqSort []     = []
quiqSort (x:xs) = (quiqSort (filter (<x) xs)) ++ (x : filter (== x) xs) ++ (quiqSort (filter (>x) xs))