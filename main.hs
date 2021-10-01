getDirectoryContents "."module Main where

import Data.List

names = [
  ("Bernard", "Samner"),
  ("Yen", "Kertis"),
  ("Selena", "Huk"),
  ("Piter", "Huk"),
  ("Stiven", "Morris")
  ]

compareNames getName p p' = compare (getName p) (getName p')

compareNameAndSurname a b
  | r /= EQ   = r
  | otherwise = compareNames fst a b
  where
    r = compareNames snd a b
  

main = print $ sortBy compareNameAndSurname names
-- main = print $ binary_search [1] 1

binary_search :: Ord a => [a] -> a -> Maybe Int
binary_search xs x = helper 0 high where
    high = length xs - 1
    helper low high
      | low > high = Nothing
      | mx < x  = helper (mid + 1) high
      | mx > x  = helper low (mid - 1)
      | mx == x = Just mid
      where
        mid = (high + low) `div` 2
        mx = xs !! mid




sfOffice name = nameText ++ " - " ++ address
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName
    address = if lastName < "L" 
      then "Sf, 94111" 
      else "Sf, 94109"

nyOffice name = nameText ++ ": NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - Reno, 89523"
  where nameText = snd name

klOffice name = nameText ++ " - KL, 23312"
  where nameText = "!!!" ++ (fst name) ++ " " ++ (snd name)

getLocationFunc location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "kl" -> klOffice
    _ -> (\n -> (fst n) ++ " " ++ (snd n))

addressLetter name location = locationFunc name
  where locationFunc = getLocationFunc location
    


