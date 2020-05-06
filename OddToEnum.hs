data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

instance Enum Odd where
  succ = (`addEven` 2)
  pred = (`addEven` (-2))
  enumFrom x = x : enumFrom (succ x)
  enumFromThen (Odd x) (Odd y) = Odd x : enumFromThen (Odd y) (Odd (y + (y - x)))
  enumFromTo a@(Odd x) b@(Odd y) = if x <= y then a : enumFromTo (succ a) b else []
  enumFromThenTo a@(Odd x) b@(Odd y) c@(Odd z) = let
      step = y - x
      next = if step < 0 then x >= z else x <= z
    in if next then a : enumFromThenTo (Odd (x + step)) (Odd (y + step)) c else []