module ListFunctions where

listFuncTake = take 5 "Hello, World!" -- "Hello"
listFuncTake' = take 3 [1, 2, 3, 4, 5] -- [1, 2, 3]

listFuncDrop = drop 7 "Hello, World!" -- "World!"
listFuncDrop' = drop 2 [1, 2, 3, 4] -- [3, 4]

listSplitAt = splitAt 7 "Hello, World!" -- ["Hello, ", "World!"]

fiveItemFromList = "Hello!" !! 5 -- "!"

listFuncFilter = filter (< 3) [1, 2, 3, 4, 5, 6, 4, 1 ,2] -- [1, 2, 1, 2]
listFuncTakeWhile = takeWhile (< 3) [1, 2, 1, 5, 3, 1, 2] -- [1, 2, 1]
listFuncDropWhile = dropWhile (< 3) [1, 2, 1, 5, 3, 1, 2] -- [5, 3, 1, 2]
listFuncSpan = span (< 3) [1, 2, 3, 4, 1] -- ([1,2], [3,4,1])
listFuncBreak = break (> 3) [1, 2, 3, 4, 5, 6, 1, 2] -- ([1,2,3], [4,5,6,1,2])

listFuncMap = map length ["aa", "bbb", "ccccccc"] -- [2, 3, 7]
listFuncConcat = concat [["Hello", ", ", "world", "!"]] -- "Hello, world!"
listFuncConcatMap = concatMap (\x -> [x, x, x]) "ABC" -- "AAABBBCCC"

listFuncAnd = and [True, True, False] -- False
listFuncAnd' = and [True, True] -- True

listFuncOr = or [True, False] -- True
listFuncOr' = or [False, False] -- False

listFuncAll = all odd [1,2,3] -- False
listFuncAll' = all odd [1,3,5] -- True

listFuncAny = any odd [1,2,3] -- True
listFuncAny' = any even [1,3,5] -- False

funcRepeat = take 5 $ repeat 'z' -- 'zzzzz'
funcReplicate = replicate 5 'z' --  'zzzzz'
funcCycle = take 7 $ cycle [1,2,3] -- [1,2,3,1,2,3,1]
funcIterate = take 5 $ iterate (^2) 2 -- [2,4,16,256,65536]

range1To10 = [1..10]
range1To10' = enumFromTo 1 10
rangeAToZ = ['a'..'z'] -- "abcdefghijklmnopqrstuvwxyz"
oddList = [1,3..10] -- [1,3,5,7,9]
evenList = enumFromThenTo 0 2 10 -- [0,2,4,6,8,10]
fiveNats = take 5 $ [1..] -- [1,2,3,4,5]
fiveNats' = take 5 $ enumFrom 1
fiveOdd = take 5 $ [1,3..]
fiveOdd' = take 5 $ enumFromThen 1 3