import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k,v)] -> m k v
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
  deriving (Eq,Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup key (ListMap list) = foldr (\(k,v) s -> if k == key then Just v else s) Nothing list
  insert key val (ListMap list) = helper [] list where
    helper r [] = ListMap (r ++ [(key,val)])
    helper r (p@(k,v):xs) = if k == key then ListMap (r ++ [(key,val)] ++ xs) else helper (r ++ [p]) xs
  delete key (ListMap list) = ListMap (filter (\(k,v) -> k /= key) list)