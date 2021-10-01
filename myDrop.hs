myDrop n xs    | n <= 0 = xs
myDrop _ []             = []
myDrop n (_:xs)         = myDrop (n-1) xs