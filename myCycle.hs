myCycle [] = error "empty list"
myCycle xs = xs ++ myCycle xs