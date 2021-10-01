fastFib n = helper 1 1 n
  where
    helper _ _ c | c <= 2 = 1
    helper x y c          = x + helper y (x + y) (c - 1)