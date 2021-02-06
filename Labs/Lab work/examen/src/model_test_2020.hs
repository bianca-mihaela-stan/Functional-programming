{-# LANGUAGE FlexibleInstances #-}
module Model where
import           Data.Char      (isUpper)
import           Data.Foldable  (foldMap, foldr)
import           Data.Monoid
import           Data.Semigroup (Max (..), Min (..))
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
--linia lista coordonatelor punctului sa coincida cu frontiera arborelui.

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


