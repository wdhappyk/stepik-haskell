module Classes where
  
-- class Eq a where
--     (==), (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)

-- instance Eq Bool where
--     True  == True  = True
--     False == False = True
--     _     == _     = False

-- instance (Eq a, Eq b) => Eq (a, b) where
--     p1 == p2 = fst p1 == fst p2 && snd p1 == snd p2

class Printable a where
  toString :: a -> [Char]

instance Printable Bool where
  toString True  = "true"
  toString False = "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString p = "(" ++ s1 ++ "," ++ s2 ++ ")" where
      s1 = toString $ fst p
      s2 = toString $ snd p

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab a | doesEnrageGork a && doesEnrageMork a = (stomp . stab) a
                | doesEnrageGork a = stab a
                | doesEnrageMork a = stomp a
                | otherwise = a

                
class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x = if maxBound == x then minBound else succ x

  spred :: a -> a
  spred x = if minBound == x then maxBound else pred x