{-# LANGUAGE FlexibleInstances #-}
module Lab where
import           Data.Char      (isUpper)
import           Data.Foldable  (foldMap, foldr)
import           Data.Monoid
import           Data.Semigroup (Max (..), Min (..))


data Arbore a = Nod (Arbore a) a (Arbore a) | Frunza a | Nil

--a

get_value :: Arbore a -> a
get_value (Nod left x right) = x
get_value (Frunza x) = x

get_max  :: (Ord a) => Arbore a -> a
get_max (Frunza x) = x
get_max (Nod left x Nil) = x 
get_max (Nod left x right) = max x (get_max right)

get_min :: (Ord a) => Arbore a -> a
get_min (Frunza x) = x
get_min (Nod Nil x right) = x
get_min (Nod left x right) 
    | x < get_min left = x
    | otherwise = get_min left


verificare  :: (Ord a) => Arbore a -> Bool 
verificare (Frunza x) = True
verificare (Nod left x right) = x > get_max left && x < get_min right

--teste facute:
-- verificare (Frunza 6)
-- True
-- verificare (Nod (Frunza 4) 5 (Frunza 6))
-- True
-- verificare (Nod (Frunza 4) 5 (Frunza 5))
-- False
-- verificare (Nod (Frunza 4) 5 (Nod (Frunza 3) 6 (Frunza 7)))
-- False

--b
-- inserare :: (Ord a, Monoid a) => Arbore a -> a -> Arbore a
-- inserare (Frunza x) y
--     | x


------------------------------MODEL TEST 2020--------------------------------

--cu descrieri de liste
sfChr :: String -> Int
sfChr text = sum [1 | x <- text, x=='.' || x=='?' || x=='!' || x==':']

--cu recursie
sfChr' :: String -> Int
sfChr' [] = 0
sfChr' (x:xs)
    | x=='.' || x=='?' || x=='!' || x==':' = sfChr' xs+1
    | otherwise = sfChr' xs

--cu functii de nivel inalt
sfChr'' :: String -> Int
sfChr'' text =  sum (fmap (\x -> if  x=='.' || x=='?' || x=='!' || x==':' then 1 else 0) text)

------------------------------Execitiul 2---------------------------------
liniiN :: [[Int]] -> Int -> Bool 
liniiN matrice n= foldr (&&) True [ foldr (&&) True (fmap (\x -> x>0) linie) | linie <-matrice, sum (fmap (\x -> 1) linie) == n]

liniiN' :: [[Int]] -> Int -> Bool 
liniiN' matrice n = foldr (&&) True (fmap (\x -> x > 0) (concat (filter (\x -> length x ==n) matrice)))

--teste facute:
-- liniiN [[1,2,3], [1], [2]] 3
-- True
-- liniiN [[1,2,3], [-1], [2]] 1
-- False
-- liniiN [[1,2,3], [1], [2]] 1  
-- True
------------------------------EXERCITIUL 3-------------------------------

--Instanca a clasei ToFromArb pentru tipul de date punct astfel incat
--linia lista coordonatelor punctului sa coincida cu frontiere arborelui.

data Punct = Pt [Int] deriving Show

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

concaten :: Punct -> Punct -> [Int]
concaten (Pt x) (Pt y) = x ++ y

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N left right) = Pt ( concaten (fromArb left) (fromArb right))


