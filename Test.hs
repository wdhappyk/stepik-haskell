module Test where

import Data.List
import Data.Char

fibonacci :: Integer -> Integer
fibonacci = let
        helper n1 n2 0 = n1
        helper n1 n2 1 = n2
        helper n1 n2 (-1) = n2
        helper n1 n2 n | n > 0 = helper n2 (n2 + n1) (n - 1)
                       | n < 0 = helper (-n2) (-(n2 + n1)) (n + 1)
    in helper 0 1

seqA :: Integer -> Integer
seqA k = let
            helper k0 k1 k2 n | n == 0 = k0
                              | n == 1 = k1
                              | n == 2 = k2
                              | n >= 3 = (helper k1 k2 $! (k2 + k1 - 2 * k0)) (n - 1)
        in helper 1 2 3 k


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (summator x, len x) where
    len x = counter 0 (abs x) where
        counter count n | n `div` 10 >= 1 = counter (count + 1) (n `div` 10)
                        | otherwise = count + 1
    summator x = counter 0 (len (abs x)) (abs x) where
        counter sum len n | n >= 10 = let
                                        k = 10 ^ (len - 1)
                                    in counter (sum + n `div` k) (len - 1) (n - (n `div` k) * k)
                        | otherwise = sum + n


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = step * (abs func) where
    n = 1000000
    step = (b - a) / n
    func = (f a + f b) / 2 + sumF 0 1 a where
        sumF res counter val | counter < n = sumF (res + f val) (counter + 1) (val + step)
                             | otherwise = res

on :: (a -> a -> a) -> (a -> a) -> a -> a -> a
on op f x y = f x `op` f y

sumSquare :: Integer -> Integer -> Integer
sumSquare = (+) `on` (^2)

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f p = f (fst p) (snd p)

avg :: Int -> Int -> Int -> Double
avg x y z =  (realToFrac x + realToFrac y + realToFrac z) / 3

nTimes:: a -> Int -> [a]
nTimes = repeat [] where
    repeat l i n | n > 0 = repeat (i : l) i (n - 1)
                 | otherwise = l


-- oddsOnly :: Integral a => [a] -> [a]
-- oddsOnly = helper [] where
--     helper r [] = r
--     helper r (x : xs) | odd x = helper (x ++ [r]) xs
--                       | otherwise = helper r xs
oddsOnly :: Integral a => [a] -> [a]
oddsOnly = filter odd

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome (x : xs) | x == last xs = (isPalindrome . init) xs
                      | otherwise = False


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 = helper [] where
    helper r [] [] [] = r
    helper r l1 l2 l3 = helper (r ++ [getN l1 + getN l2 + getN l3]) (getT l1) (getT l2) (getT l3)
    getN []      = 0
    getN (x : _) = x
    getT [] = []
    getT l = tail l

groupElems :: Eq a => [a] -> [[a]]
groupElems = helper [] [] where
    helper r [] [] = r
    helper r cur [] = r ++ [cur]
    helper r cur (x : xs) | cur == [] || head cur == x = helper r (x : cur) xs
                          | otherwise = helper (r ++ [cur]) [x] xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort xs@(x:_) = qsort (filter (< x) xs) ++ (filter (== x) xs) ++ qsort (filter (> x) xs)

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms xs@[_,_] = [xs, reverse xs]
perms xs = concatMap (\(x:xs) -> map (\a -> x : a) (perms xs)) (each [] 0 xs) where
    toFirst n xs = (xs !! n) : take n xs ++ drop (n + 1) xs
    len = length xs
    each r n xs | n < length xs = each (r ++ [toFirst n xs]) (n + 1) xs
                | otherwise = r

-- import Data.Char
delAllUpper :: String -> String
delAllUpper = unwords . (filter (any isLower)) . words

fibStream :: [Integer]
fibStream = helper [0,1] where
    helper xs@(x:t) = x : helper (t ++ zipWith (+) t xs)

-- data Odd = Odd Integer 
--   deriving (Eq, Show)

-- addEven :: Odd -> Integer -> Odd
-- addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
--                 | otherwise      = error "addEven: second parameter cannot be odd"

-- instance Enum Odd where
--     succ = (`addEven` 2)

change :: (Ord a, Num a) => a -> [[a]]
change n = [li | li <- concat [helper [x] | x <- coins, x <= n], sum li == n] where
    coins :: (Ord a, Num a) => [a]
    coins = [2,3,7]
    helper xs = [xs] ++ concat [helper (i:xs) | i <- coins, sum (i:xs) <= n]


-- evenOnly :: [a] -> [a]
-- evenOnly xs = foldr (\x s -> (xs !! x):s) [] [1,3..(length xs - 1)]
evenOnly :: [a] -> [a]
evenOnly = foldr (\(i,x) s -> if odd i then s else x:s) [] . zip [1..]

flipConst x y = flip const x y -- == const y x

scanlExample = scanl (+) 1 [1..10] -- [1,2,4,7,11,16,22,29,37,46,56]
                                   -- все промежуточные результаты левой свертки
scanrExample = scanr (+) 1 [1..10] -- [56,55,53,50,46,41,35,28,20,11,1]
                                   -- все промежуточные результаты правой свертки

unfold :: (b -> (a,b)) -> b -> [a]
unfold f ini = let (x, ini') = f ini in
    x : unfold f ini'

-- custom iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\x -> (x, f x))

iterateExample = take 5 $ iterate' (^2) 2 -- [2,4,16,256,65536]

unfoldrExample = take 5 $ unfoldr (\x -> Just (x, x^2)) 2 -- [2,4,16,256,65536]

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g (a,b) = if a > b then Nothing else Just (b,(a, pred b))

data B = T | F deriving (Show, Eq, Enum, Read)

not' :: B -> B
not' T = F
not' F = T


data Color = Red | Green | Blue
instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"


-- data Result = Fail | Success
-- doSomeWork :: SomeData -> (Result,Int)
-- processData :: SomeData -> String
-- processData x = case doSomeWork x of
--     (Fail, n) -> "Fail: " ++ show n
--     (Success, _) -> "Success"


-- Конструктор произведений
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') = distanceToOrigin (Point (x-x') (y-y'))

distanceExample = distanceToOrigin (Point 3.0 4.0) -- 5.0


-- Тип данных суммы произведений
data Roots = Roots Double Double | None
    deriving Show

roots :: Double -> Double -> Double -> Roots
roots a b c
    | discr >= 0 = Roots x1 x2
    | otherwise  = None
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c



data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b


-- data Result' = Fail Int | Success

-- instance Show Result' where
--     show (Fail n) = "Fail: " ++ show n
--     show Success  = "Success"

-- doSomeWork' :: SomeData -> Result'
-- doSomeWork' x = case doSomeWork x of
--     (Fail,    n) -> show (Fail n)
--     (Success, _) -> show Success
        
-- Более краткое решение
-- data Result' = Result' (Result, Int)

-- instance Show Result' where
--     show (Result' (Fail, n))    = "Fail: " ++ show n
--     show (Result' (Success, _)) = "Success"
-- doSomeWork' :: SomeData -> Result'
-- doSomeWork' = Result' . doSomeWork


-- data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _               = False



data Coord a = Coord a a --deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter s (Coord x y) = let n = s/2 in Coord (fromIntegral x * s + n) (fromIntegral y * s + n)

getCell :: Double -> Coord Double -> Coord Int
getCell s (Coord x y) = let
                            x' = x / s
                            y' = y / s
                        in Coord (floor x') (floor y')


roots' :: Double -> Double -> Double -> Either [Char] (Double, Double)
roots' a b c
    | discr >= 0 = Right (x1, x2)
    | otherwise  = Left "Negative discriminant"
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c
