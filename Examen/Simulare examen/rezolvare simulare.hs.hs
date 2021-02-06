module Simulare where
import Data.Monoid ( Sum(Sum, getSum) )
import Data.Map ()
import Data.List

data Arbore a = Nod (Arbore a) a (Arbore a) | Frunza a | Nil

------------------------------EXERCITIUL 1-----------------------------

------------------------------VERIFICARE------------------------------

-- get_max  :: (Ord a) => Arbore a -> a
-- get_max (Frunza x) = x
-- get_max (Nod left x Nil) = x 
-- get_max (Nod left x right) = max (max x (get_max right)) (get_max left)

-- get_min :: (Ord a) => Arbore a -> a
-- get_min (Frunza x) = x
-- get_min (Nod Nil x right) = x
-- get_min (Nod left x right) = min (min x (get_min right)) (get_min left)

--------------------------------------------------------------------------
--Prima varianta de implementare, cu foldr-ul definit la 3 a) 
--------------------------------------------------------------------------

verificare  :: (Ord a) => Arbore a -> Bool 
verificare Nil = True
verificare (Frunza x) = True
verificare (Nod left x right) 
    | length right_list >0  && length left_list>0 = x < minimum right_list && x > maximum left_list && verificare left && verificare right
    | length right_list >0 = x < minimum right_list && verificare left && verificare right
    | length left_list>0 = x > maximum left_list && verificare left && verificare right
    | otherwise = verificare left && verificare right
    where
        right_list =  (foldr (:) [] right)
        left_list = (foldr (:) [] left)

-------------------------------------------------------------------
--A doua varianta de implementare, in caz ca nu aveam voie cu foldr-ul
--------------------------------------------------------------------

mai_mare :: (Ord a) => a -> Arbore a -> Bool 
mai_mare x Nil = True 
mai_mare x (Frunza y) = x > y
mai_mare x (Nod left y right) = mai_mare x left && x>y && mai_mare x right

mai_mic :: (Ord a) => a -> Arbore a -> Bool 
mai_mic x Nil = True 
mai_mic x (Frunza y) = x < y
mai_mic x (Nod left y right) = mai_mic x left && x<y && mai_mic x right

verificare' :: (Ord a) => Arbore a -> Bool 
verificare' Nil = True 
verificare' (Frunza x) = True 
verificare' (Nod left x right) = mai_mare x left && mai_mic x right && verificare' left && verificare' right

--teste facute:
-- verificare (Frunza 6)
-- True
-- verificare (Nod (Frunza 4) 5 (Frunza 6))
-- True
-- verificare (Nod (Frunza 4) 5 (Frunza 5))
-- False
-- verificare (Nod (Frunza 4) 5 (Nod (Frunza 3) 6 (Frunza 7)))
-- False

------------------------------INSERT-------------------------------------

insert :: (Ord a) => Arbore a -> a -> Arbore a
insert Nil x = Nod Nil x Nil
insert (Frunza x) y 
    | x<y = Nod Nil x (Frunza y)
    | x>y = Nod (Frunza y) x Nil
    | otherwise = Frunza x
insert (Nod left x right) y
    | x<y = Nod left x (Simulare.insert right y)
    | x>y = Nod (Simulare.insert left y) x right
    | otherwise = Nod left x right

--teste facute
test1_insert :: Bool
test1_insert = Simulare.insert (Frunza 2) 3 == Nod Nil 2 (Frunza 3)
test2_insert :: Bool
test2_insert = Simulare.insert (Frunza 4) 3 == Nod (Frunza 3) 4 Nil
test3_insert :: Bool
test3_insert = Simulare.insert (Frunza 4) 5 == Nod Nil 4 (Frunza 5)
test4_insert :: Bool
test4_insert = Simulare.insert (Nod (Frunza 1) 3 (Frunza 7)) 8 == Nod (Frunza 1) 3 (Nod Nil 7 (Frunza 8))
test5_insert :: Bool
test5_insert =  Simulare.insert (Nod (Frunza 1) 3 (Frunza 7) ) 2 == Nod (Nod Nil 1 (Frunza 2)) 3 (Frunza 7)
test6_insert :: Bool
test6_insert = Simulare.insert (Nod (Frunza 1) 3 (Frunza 7) ) 4 == Nod (Frunza 1) 3 (Nod (Frunza 4) 7 Nil)

------------------------------LIST TO ARBORE-------------------------------
list_to_arbore :: Ord a => [a] -> Arbore a 
list_to_arbore lista = foldr (funct) Nil lista
        where
            funct :: Ord a => a -> Arbore a -> Arbore a
            funct x Nil = Nod Nil x Nil
            funct y (Frunza x)  
                | x<y = Nod Nil x (Frunza y)
                | x>y = Nod (Frunza y) x Nil
                | otherwise = Frunza x
            funct y (Nod left x right) 
                | x<y = Nod left x (funct y right)
                | x>y = Nod (funct y left) x right
                | otherwise = Nod left x right

-------------------------------DELETE RADACINA---------------------------------
delete :: (Ord a) => Arbore a -> Arbore a
delete (Frunza x) = Nil
delete (Nod Nil x right) = right
delete (Nod left x Nil) = left
delete (Nod left x right) = Nod left y right
    where
        y = leftistElement right

leftistElement :: (Ord a) => Arbore a -> a
leftistElement (Nod Nil v _) = v
leftistElement (Nod t1 _ _) = leftistElement t1

-----------------------------DELETE---------------------------------

delete_node :: (Ord a) => Arbore a -> a -> Arbore a
delete_node arb a = list_to_arbore (filter (\x -> x/= a) (foldr (:) [] arb))



-------------------------------------FUNCTOR----------------------------

instance Functor Arbore where
    --fmap :: a-> b -> Arbore a -> Arbore b
    --f :: a -> b
    fmap f Nil = Nil
    fmap f (Frunza x)= Frunza (f x)
    fmap f (Nod left x right) = Nod (fmap f left) (f x) (fmap f right)

test1_functor :: Bool
test1_functor = fmap (+3) (Nod (Frunza 1) 7 (Nod Nil 10 (Frunza 11))) == Nod (Frunza 4) 10 (Nod Nil 13 (Frunza 14))
test2_functor :: Bool 
test2_functor = fmap (+3.0) (Nod (Frunza 1) 7 (Nod Nil 10 (Frunza 11))) == Nod (Frunza 4.0) 10.0 (Nod Nil 13.0 (Frunza 14.0))


-------------------------------APPLICATIVE------------------------------
--Aici am facut eu o instanta de Applicative mai stupida, dar voiam sa 
--exersez.

instance Applicative Arbore where
    pure x = Frunza x
    -- <*> :: Arbore (a->b) -> Arbore a -> Arbore b
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    (Frunza f) <*> (Frunza x) = Frunza (f x) 
    (Frunza f) <*> (Nod left x right) = Nod ((Frunza f) <*> left) (f x) (Frunza f <*> right)
    (Nod f_left f f_right) <*> (Nod left x right) = Nod ((Frunza f) <*> left) (f x) (Frunza f <*> right)

test1_applicative :: Bool
test1_applicative = ((Frunza (+3)) <*> (Nod (Frunza 1) 4 Nil)) == Nod (Frunza 4) 7 Nil

-------------------------------FOLDABLE----------------------------------
instance Foldable Arbore where
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


-- instance Eq (Reteta) where
--     Stop == Stop = True 
--     Stop == R a b == False 
--     R a b == Stop = False 
--     //R a b == R c d = mhkash

    --in cazul nostru avem
    --foldr :: Foldable t => (a -> b -> b) -> b -> Arbore a -> b
    -- foldr f z Nil = z
    -- foldr f z (Frunza x) = f x z
    -- foldr f z (Nod left x right) = foldr f (f x (foldr f z right)) left

test1_foldr :: Bool
test1_foldr = foldr (:) [] (Nod (Frunza 'a') 'b' (Frunza 'c')) == "abc"
test2_foldr :: Bool
test2_foldr = foldr (:) [] (Frunza 'a') == "a"
test3_foldr :: Bool
test3_foldr = foldr (:) [] Nil == ""

test1_foldMap :: Bool
test1_foldMap = foldMap (\x -> Sum x)  (Nod (Frunza 1) 2 (Frunza 6)) == Sum {getSum = 9}
test2_foldMap :: Bool
test2_foldMap = foldMap (\x -> Sum x)  (Nil) == Sum {getSum = 0}
test3_foldMap :: Bool
test3_foldMap = foldMap (\x -> Sum x)  (Frunza 1) == Sum {getSum = 1}


instance (Ord a, Show a)=> Show (Arbore a) where
    show arb
        | verificare' arb == True = show lista
        | otherwise = "Arborele nu verifica proprietatea de arbore de cautare!"
        where 
            lista = (foldr (:) [] arb)

test1_show :: Bool
test1_show = show (Frunza 1) == "[1]"
test2_show :: Bool
test2_show = show (Nil::Arbore Int) == "[]"
test3_show :: Bool 
test3_show = show (Nod (Frunza 1) 4 (Nod (Frunza 5) 7 (Nod (Frunza 8) 9 Nil))) == "[1,4,5,7,8,9]"
test4_show :: Bool
test4_show = show (Nod (Frunza 1) 4 (Nod (Frunza 5) 7 (Nod (Frunza 6) 9 Nil))) == "Arborele nu verifica proprietatea de arbore de cautare!"
--daca scriu in interpreatator Frunza 'a' o sa imi afiseze "a"

--------------------------EQ-------------------------------
instance (Ord a) => Eq (Arbore a) where
    arb1 == arb2 = sort (foldr (:) [] arb1) == sort (foldr (:) [] arb2)

test1_eq :: Bool
test1_eq = ((Nil::Arbore Int) == Nil) == True
test2_eq :: Bool
test2_eq = (Frunza 4 == Nod Nil 4 Nil) == True
test3_eq :: Bool
test3_eq = ( Frunza 4 == Nod Nil 3 Nil) == False
test4_eq :: Bool
test4_eq = (Nod (Frunza 1) 5 (Nod Nil 6 Nil) == Nod (Nod Nil 1 Nil) 5 (Frunza 6)) == True

---------------------------------ORD-------------------------------

instance (Ord a) =>Ord (Arbore a) where
    Nil <= Nil = True 
    Nil <= Frunza x = True 
    Frunza x <= Frunza y = x<=y
    Frunza x <= Nod Nil y Nil = x <= y
    Nod Nil x Nil <= Frunza y = x <= y
    Nod left_x x right_x <= Nod left_y y right_y = x <= y
    Nil <= Nod left_x x right_x = True 

------------------------------SEMIGROUP--------------------------------

instance (Ord a) => Semigroup (Arbore a) where
    -- <> :: Arbore a -> Arbore b -> Arbore c
    -- a <> Nil = a
    -- Nil <> a = a
    -- Frunza x <> Frunza y = Nod Nil (min x y) (Frunza (max x y)) 
    -- Nod left x right <> Frunza y 
    --     | x > y = Nod (left <> Frunza y) x right
    --     | x < y = Nod left x (right <> Frunza y)
    --     | otherwise = Nod left x right
    -- Frunza y <> Nod left x right 
    --     | x > y = Nod (left <> Frunza y) x right
    --     | x < y = Nod left x (right <> Frunza y)
    --     | otherwise = Nod left x right
    --(arb_x ++ arb_y) :: [a]
    -- foldr :: (a -> Arbore a -> Arbore) -> Arbore b -> [a] -> Arbore b
    -- foldr :: (a -> Arbore a -> Arbore a) -> 
    -- fmap :: a-> b -> Arbore a -> Arbore b
    arb1 <> arb2 = foldr (funct) Nil ((foldr (:) [] arb1) ++ foldr (:) [] arb2)
        where
            funct :: Ord a => a -> Arbore a -> Arbore a
            funct x Nil = Nod Nil x Nil
            funct y (Frunza x)  
                | x<y = Nod Nil x (Frunza y)
                | x>y = Nod (Frunza y) x Nil
                | otherwise = Frunza x
            funct y (Nod left x right) 
                | x<y = Nod left x (funct y right)
                | x>y = Nod (funct y left) x right
                | otherwise = Nod left x right

test1_semigroup = (Frunza 1 <> Frunza 2) == Nod Nil 1 (Frunza 2)

instance Ord a => Monoid (Arbore a) where
    mempty = Nil