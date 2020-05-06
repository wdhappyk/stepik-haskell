data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = (\a -> Log [msg] (f a))

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = (helper . f) x where
  helper (Log ls v) = let
      (Log msg r) = g v
    in Log (ls ++ msg) r


add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

res = execLoggers 3 add1Log mult2Log -- Log ["added one","multiplied by 2"] 8


returnLog :: a -> Log a
returnLog x = Log [] x


bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log ls x) f = let
    (Log msg r) = f x
  in Log (ls ++ msg) r


instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a fs = foldl (\s f -> s >>= f) (return a) fs

-- execLoggersList a fs = foldl helper (Log [] a) fs where
--   helper (Log ls v) f = let
--       (Log msg r) = f v
--     in Log (ls ++ msg) rs