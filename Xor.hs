module Xor where

import Data.Monoid
newtype Xor = Xor { getXor :: Bool }
  deriving (Eq,Show)

instance Monoid Xor where
  mempty = Xor False
  mappend x y = Xor (x /= y)