data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

fromBit :: Bit -> Int
fromBit Zero = 0
fromBit One = 1

toBit :: Int -> Bit
toBit 0 = Zero
toBit 1 = One
toBit _ = undefined

signInt :: Int -> Sign
signInt x = if x < 0 then Minus else Plus

toInt :: Z -> Int
toInt (Z _ []) = 0
toInt (Z sign bits) = let
    bs = map fromBit bits
    k = case sign of
      Minus -> -1
      Plus -> 1
  in (foldr (\x s -> s*2+x) 0 bs) * k

fromInt :: Int -> Z
fromInt x = Z (signInt x) (helper [] (abs x)) where
  helper r x = let
      (n,m) = x `quotRem` 2
      r' = r ++ [toBit m]
    in if n == 0 then r' else helper r' n

add :: Z -> Z -> Z
add x y = fromInt $ (toInt x) + (toInt y)

mul :: Z -> Z -> Z
mul x y = fromInt $ (toInt x) * (toInt y)