{-# LANGUAGE BlockArguments #-}
module Lib where
import Control.Monad.Reader 
import Data.Char 
import Data.Foldable 
import Data.Monoid 
import Data.Semigroup 
import Data.List 
import Test.QuickCheck 


data Reteta = Stop | R Ing Reteta
    deriving Show

data Ing = Ing String Int
    deriving Show

data Sbr06Arbore a= Nod (Sbr06Arbore a) a (Sbr06Arbore a) | Frunza a | Nil


instance Foldable Sbr06Arbore where
    --foldMap :: (Foldable t, Monoid m) => (a->m) -> t a -> m
    --aplica f pe structura si apoi combina structura intr-un singur element prin monoid
    foldMap f Nil = mempty 
    foldMap f (Frunza x) = f x
    foldMap f (Nod left x right) = foldMap f left `mappend` f x `mappend` foldMap f right

    --pot sa definesc si foldr daca vreau
    --foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    --f :: (a -> b-> b) : o functie pe care o aplic pe 2 argumente, unul de tip a si unul de tip b
    --z :: b chestia de baza de la care plec
    --  :: t a structura pe care aplic
    --  :: b rezultatul

    --in cazul nostru avem
    --foldr :: Foldable t => (a -> b -> b) -> b -> Arbore a -> b
    -- foldr f z Nil = z
    -- foldr f z (Frunza x) = f x z
    -- foldr f z (Nod left x right) = foldr f (f x (foldr f z right)) left



sbr06list_to_arbore :: [(String, Int)] -> Sbr06Arbore (String, Int) 
sbr06list_to_arbore lista = foldr (sbr06funct) Nil lista
        where
            sbr06funct :: (String, Int) -> Sbr06Arbore (String, Int) -> Sbr06Arbore (String, Int)
            sbr06funct x Nil = Nod Nil x Nil
            sbr06funct y (Frunza x)  
                | map toUpper(fst x) <map toUpper ( fst y) = Nod Nil x (Frunza y)
                | map toUpper (fst x)> map toUpper (fst y) = Nod (Frunza y) x Nil
                | otherwise = Frunza (fst x, max (snd x) (snd y))
            sbr06funct y (Nod left x right) 
                |  map toUpper(fst x) <map toUpper ( fst y) = Nod left x (sbr06funct y right)
                | map toUpper (fst x)> map toUpper (fst y) = Nod (sbr06funct y left) x right
                | otherwise = Nod left (fst x, max (snd x) (snd y)) right

sbr06arboreToList :: Sbr06Arbore (String, Int) -> [(String, Int)]
sbr06arboreToList arb = (foldr (:) [] arb)

--a
sbr06get_nume :: Ing -> String
sbr06get_nume (Ing a b) = a

sbr06get_cantitate :: Ing -> Int
sbr06get_cantitate (Ing a b) = b

sbr06ajutatoare :: Reteta -> [(String, Int)]
sbr06ajutatoare Stop = []
sbr06ajutatoare (R ingred restul_retetei) = (sbr06get_nume ingred, sbr06get_cantitate ingred):(sbr06ajutatoare restul_retetei)

sbr06ListToReteta :: [(String, Int)] -> Reteta
sbr06ListToReteta [] = Stop
sbr06ListToReteta (x:xs) = R (Ing (fst x) (snd x)) (sbr06ListToReteta xs)

sbr06 :: Reteta -> Reteta
sbr06 Stop = Stop
sbr06 reteta = sbr06ListToReteta (sbr06arboreToList (sbr06list_to_arbore (sbr06ajutatoare reteta)))
-- --b
-- instance Eq (Reteta)


--3 a

--fTest0 = 

data E x = A | M x Bool (E x)
instance Foldable (E) where
    foldr funct nil A = nil
    foldr funct nil (M x bool another_e) = (funct x (foldr funct nil another_e))



--3 b
class C e where
  cFilter :: (a -> Bool) -> e a -> e a
  toList :: e a -> [a]

instance C (E) where
    cFilter predicat A = A
    cFilter predicat (M x bool another_e)
        | predicat x == False = (M x False (cFilter predicat another_e))
        | otherwise = (M x bool (cFilter predicat another_e))

    toList A = []
    toList (M x bool another_e)
        | bool == True = x : (Lib.toList another_e)
        | otherwise = (Lib.toList another_e)
--cTest0 = toList  (cFilter (>2) (M 1 True (M 5 False (M 3 True (M 2 True A))))) == [3]
-- cTest1 = cFilter (\x -> x `div` 2==0) (M 1 True (M 5 False (M 3 True (M 2 True A)))) == []
-- --2 c


-- sbr06arboreToReteta :: Arb -> Reteta
-- sbr06arboreToReteta (Leaf x y) = (R (Ing y x) (Stop))
-- sbr06arboreToReteta (Node arb_left x y arb_right) = 


instance Eq (Reteta) where
    Stop == Stop = True 
    Stop == R a b = False 
    R a b == Stop = False 
    reteta1 == reteta2 = sort (sbr06ajutatoare reteta1) == sort(sbr06ajutatoare reteta2)

r1 =  R (Ing "faina" 500) (R (Ing "oua" 4) (R  (Ing "zahar" 500) (R (Ing "faina" 300) Stop)))
r2 =  R (Ing "fAIna" 500) (R (Ing "zahar" 500) (R (Ing "Oua" 4) Stop ))
r3 =  R (Ing "fAIna" 500) (R (Ing "zahar" 500) (R  (Ing "Oua" 55) Stop))

data Arb = Leaf Int String | Node Arb Int String Arb
    deriving Show 
