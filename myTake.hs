myTake n xs    | n <= 0 = []
myTake _ []             = []
myTake n (x:xs)         = x : myTake (n-1) xs