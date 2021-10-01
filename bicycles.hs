
myMax :: Ord a => a -> a -> a
myMax a b = if a >= b then a else b

myMaxLs :: Ord a => [a] -> Maybe a
myMaxLs [] = Nothing
myMaxLs (x:xs) = Just (foldr (\n i -> myMax i n) x xs)





