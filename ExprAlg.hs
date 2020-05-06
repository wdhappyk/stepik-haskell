infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = let
    r = expand e1 :*: expand e2
    next = case r of
      ((e1 :+: e2) :*: e) -> True
      (e :*: (e1 :+: e2)) -> True
      (e1 :+: e2) -> True
      (e1 :*: e2) -> True
      _ -> False
  in if next then r else expand r
expand e = e

