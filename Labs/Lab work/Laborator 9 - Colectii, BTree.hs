import qualified Data.List as List
import           Prelude   hiding (lookup)


-- Polimorfism in Haskell:
--   - parametric:
--      ex: f :: a -> a
--          f x = x -- ( vezi id )
--          data [a] = [] | a : [a]
--          data Maybe a = Just a | Nothing
--   - ad-hoc (sau constrained/bounded):
--      ex: class Show a where
--              show :: a -> String
-- ex: f :: Num a => a -> a
--     f x = x + x

f :: a -> a
f x = x  -- x -- ( vezi id )

f' :: Num a => a -> a
f' x = x + x

-- a :: *
-- a :: Type
-- class Eq a where
--     (==) :: a -> a -> Bool
--
-- data Maybe a = Just a | Nothing
-- Just :: a -> Maybe a
--
-- -- kind --
-- * e echivalent cu Type
-- Maybe :: * -> *

-- c :: * -> * -> *
-- c :: Type -> Type -> Type
class Collection c where
    empty :: c key value

    singleton :: key -> value -> c key value

    insert
        :: Ord key
        => key
        -> value
        -> c key value
        -> c key value

    lookup :: Ord key => key -> c key value -> Maybe value

    delete :: Ord key => key -> c key value -> c key value

    keys :: c key value -> [key]
    keys = map fst . toList
    -- echivalent cu
    -- keys collection = map fst (toList collection)

    values :: c key value -> [value]
    values = undefined

    toList :: c key value -> [(key, value)]

    fromList :: Ord key => [(key,value)] -> c key value
    fromList = foldr (uncurry insert) empty

-- implementati voi uncurry :: (a -> b -> c) -> (a, b) -> c

-- f :: Int -> String -> Bool
-- f x y = ..
-- f :: (Int, String) -> Bool
-- f (x, y) = ..

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

-- newtype PairList k v = PairList [(k, v)]
-- getPairList :: PairList k v -> [(k, v)]
newtype WrapBool = WrapBool Bool

instance Collection PairList where
    -- empty :: PairList key value
    empty = PairList []
    -- singleton :: key -> value -> PairList key value
    singleton key value = PairList [(key, value)]

data SearchTree key value
  = Empty
  | Node
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    lookup _ Empty = Nothing
    lookup key (Node leftTree currentKey currentValue rightTree)
      | key < currentKey =
          lookup key leftTree
      | key > currentKey =
          lookup key rightTree
      | otherwise = currentValue

myLookup :: (Collection c, Ord k) => k -> c k v -> Maybe v
myLookup = lookup

testSearchTree :: SearchTree Int String
testSearchTree = Node Empty 5 (Just "hello") Empty

order = 1

data Element k v
  = Element k (Maybe v)
  | OverLimit

data BTree key value
  = BEmpty
  | BNode [(BTree key value, Element key value)]

