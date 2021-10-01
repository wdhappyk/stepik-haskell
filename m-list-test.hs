x3 x = [x,x,x]

list = do
  xs <- foldl (\s x -> s ++ x3 x) [] [1,2,3]
  return xs