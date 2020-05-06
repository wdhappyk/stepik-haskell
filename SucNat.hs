module SucNat  where

data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add x y = helper x y where
  helper :: Nat -> Nat -> Nat
  helper x Zero = x
  helper x (Suc t) = helper (Suc x) t

mul :: Nat -> Nat -> Nat
mul x y = helper x y where
  helper :: Nat -> Nat -> Nat
  helper acc Zero       = Zero
  helper acc (Suc Zero) = acc
  helper acc (Suc n)    = helper (add acc x) n

fac :: Nat -> Nat
fac Zero = Suc Zero
fac x@(Suc t) = mul x $! (fac t)