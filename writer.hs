import Control.Monad.Writer

type Shopping = Writer (Sum Integer, [String]) ()

purchase :: String -> Integer -> Shopping
purchase item cost = tell (Sum cost, [item])

total :: Shopping -> Integer
total = getSum . fst . snd . runWriter

items :: Shopping -> [String]
items = snd . execWriter

