module Applic where

class (Functor f) => Applicative f where
    pure :: a -> f a
    -- <*> seamana cu fmap :: (a-> b) -> f a -> f b
    -- fmap lua o functie si un functor pe tipul a si returna un functor pe tipul b
    -- <*> ia un functor pe o functie din a in b si un functor pe a si returneaza un functor pe b
    (<*>) :: f (a->b) -> f a -> f b

------------------------------MAYBE----------------------------------
--Cum arata tipul Maybe?
--data Maybe a = Just a | Nothing 

--ca la Functor, Applicative se face pe ultimul timp din scrierea lui Maybe,
--adica pe a

--Maybe stim ca e si Functor, altfel nu am putea scrie Applicative pe el.
-- => f va fi chiar Maybe.
instance Applic.Applicative Maybe where
    --daca primesc o valoare a, cum o fac de tipul Maybe? Adaug Just.
    pure a = Just a
    --Ce am zis ca primeste <*>?
    -- 1) Un functor peste o functie.
    -- 2) O valoare de tip Maybe.

    -- daca am Nothing ( a -> b) rezultatul va fi tot Nothing
    Nothing <*> _ = Nothing 
    --[ 1) ]   [2)]

    --daca am Just (a -> b) atunci fac fmap la acea functie pe fa
    (Just f) <*> fa = fmap f fa
    --[ 1) ]    [ 2)]

--teste:

test1_maybe :: Bool
test1_maybe = Just (+3) Applic.<*> Just 9 == Just 12

test2_maybe :: Bool
test2_maybe = Just (++"hahaha") Applic.<*> Nothing == Nothing 

--nu stiu de ce asta merge in interpretator dar nu aici
-- test3_maybe :: Bool
-- test3_maybe = (Nothing Applicative_implem.<*> (Just "woot")) == Nothing

test4_maybe :: Bool
test4_maybe = Applic.pure(+3)  Applic.<*> Just 10 == Just 13

test5_maybe :: Bool
-- Applicative_implem.pure (+) : se baga operatie de adunare in functorul Maybe => Just (+)        
-- Just(+) <*> Just 3 : Just (3+)
-- Just (3+) <*> Just 5 : Just 8
test5_maybe = Applic.pure (+) Applic.<*> Just 3 Applic.<*> Just 5 == Just 8

--pure f <*> x =  fmap f x
-- pure f <*> x <*> y <*> .... = fmap f x <*> y <*> ....

--------------------------INTANTA []--------------------------------
instance Applic.Applicative [] where
    --pure :: a -> ma
    -- <*> :: m (a -> b) -> ma -> mb 
    -- aka <*> :: [a->b] -> [a] -> [b]
    pure x = [x]
    fs <*> xs = [f x | f <- fs , x <- xs]

--teste:
test1_liste :: Bool
test1_liste = Applic.pure "hey"== ["hey"]
test2_liste :: Bool
test2_liste = Applic.pure "hey"== Just "Hey"
test3_liste :: Bool
test3_liste = [(*0), (+100), (^2)] Applic.<*> [1,2,3] == [0,0,0,101,102,103,1,4,9]
test4_liste :: Bool
test4_liste = [(+),(*)] Applic.<*> [1,2] Applic.<*> [3,4] == [4,5,5,6,3,4,6,8]
test5_liste :: Bool
test5_liste = (filter (>50) ( (*) <$> [2,5,10] Prelude.<*> [8,10,11])) == [55,80,100,110]  
test6_liste :: Bool
test6_liste = ((++) <$> ["ha","heh","hmm"] Prelude.<*> ["?","!","."]) == ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."] 