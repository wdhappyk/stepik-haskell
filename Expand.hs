infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = helper $ expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = helper $ expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = helper $ expand e1 :+: expand e2
expand (e1 :*: e2) = helper $ expand e1 :*: expand e2
expand e = e

helper :: Expr -> Expr
helper e = if expandable e then expand e else e

expandable :: Expr -> Bool
expandable ((e1 :+: e2) :*: e) = True
expandable (e :*: (e1 :+: e2)) = True
expandable (e1 :+: e2) = expandable e1 || expandable e2
expandable (e1 :*: e2) = expandable e1 || expandable e2
expandable e = False