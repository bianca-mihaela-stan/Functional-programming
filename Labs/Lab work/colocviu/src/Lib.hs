module Lib
where
--import Test.QuickCheck
import Data.Foldable

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- --am verificat pe ["aba"] -> True si ["aab"] -> Flase ["abc", "abba"] -> True
-- sbr1 :: [String] -> Bool
-- sbr1 [] = False
-- sbr1 list@(x:xs)
--     | x==reverse x = True
--     | otherwise = sbr1 xs

-- --zip la descrieri de liste

-- sbr2 :: [String] -> Bool
-- sbr2 list = length [x| x<-list, x==reverse x] >0

-- sbr3 :: [String] -> Bool
-- sbr3 list= length (filter (\x -> x==reverse x) list) >0

-- sbr123 :: [String] -> Bool
-- sbr123 list = sbr1 list == sbr2 list && sbr2 list == sbr3 list 


-- sbr13 :: [String] -> Bool
-- sbr13 list = sbr1 list == sbr3 list 

-- sbr4 :: [Int] -> [Int]
-- sbr4 [] = []
-- sbr4 [a] =[a]
-- sbr4 [a,b] = if a==b && a==0 then [0] else [a,b]
-- sbr4 [a,b,c] 
--     | a==b && b==c && c==0 = [0]
--     | a==b && b==0 = [0,c]
--     | b==c && c==0 =[a,0]
--     | otherwise = [a,b,c]
-- sbr4 list@(x:y:z:zs)
--     | x==y && y==0 = sbr4 (y:z:zs)
--     | otherwise = x: sbr4 (y:z:zs)

-- sbr5 :: [Int] -> [Int]
-- sbr5 []=[]
-- sbr5 [a] = [a]
-- sbr5 (x:y:ys)
--     | x==0 && y==1 = sbr5(y:ys)
--     | otherwise = x:sbr5(y:ys)


-- --am testat pe [3,0,0,1,0,2,0,0] -> [3,1,2,0]
-- --[3,0,0,1,0,2,0,0,4,5,0] -> [3,1,2,0,4,5,0]
-- --[0,0,0] -> [0]
-- --[1,0,1] -> [1,1]
-- sbr6 :: [Int] -> [Int]
-- sbr6 list = reverse eliminare_1_inainte_de_0
--     where
--         eliminare_0 = sbr4 list
--         eliminare_0_inainte_de_1 = sbr5 eliminare_0
--         eliminare_1_inainte_de_0 = sbr5 (reverse eliminare_0_inainte_de_1)


-- sbr41 :: [Int] -> [Int]
-- sbr41  [] = []
-- sbr41 [a] = [a]
-- sbr41 list@(y:ys) = [fst x| x<-m, fst x== snd x && fst x /=0 || fst x==0 && snd x /=0 || fst x /=0 && snd x ==0]
--     where 
--         m= zip list ys

-- test_sbr4_sbr41 :: [Int] -> Bool
-- test_sbr4_sbr41 list = sbr4 list == sbr41 list

-----------CURS 9--------------------
--Monoid: o multime cu o operatoie asociativa si cu element neutru
--exemple monoizi: (Int,+,0), (Int,∗, 1), (String,++, []), ({True,False},&&, True), ({True,False},||, False)
--Semigrup: monoid fara element neutru

class Semigroup  a where
    (<>) :: a -> a -> a --operatie asociativa
infixr 6 <>

class Lib.Semigroup a => Monoid a where
    mempty :: a --elemenrul neutru

    mconcat :: [a] -> a --generalizare la liste
    mconcat = Lib.foldr (Lib.<>) Lib.mempty

--asociativitate: x <> (y <> z) = (x <> y) <> z
--identitate la dreapta: x <> mempty = x
--identitate la stanga: mempty <> x = x

instance Lib.Semigroup [a] where
    (<>) = (++)
instance Lib.Monoid [a] where
    mempty = []

-----Definirea de instante diferite pentru acelasi tip
--type redenumeste tipul, newtype face o copie si permite redenumirea operatiilor

newtype All = All {getAll :: Bool}
    deriving (Eq, Read, Show)

instance Lib.Semigroup All where
    All x <> All y = All (x && y)
instance Lib.Monoid All where
    mempty = All True 

newtype Any = Any {getAny :: Bool}
    deriving (Eq, Read, Show)

instance Lib.Semigroup Any where
    Any x <> Any y = Any (x || y)
instance Lib.Monoid Any where
    mempty = Any False 

--Num a ca monoid fata de adunare

newtype Sum a = Sum {getSum :: a}
    deriving (Eq, Read, Show)

instance Num a => Lib.Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x+y)
instance Num a => Lib.Monoid (Sum a) where
    mempty = Sum 0

--Num ca monoid fata de inmultire
newtype Product a = Product {getProduct :: a}
    deriving (Eq, Read, Show)

instance Num a => Lib.Semigroup (Product a) where
    Product x <> Product y = Product (x*y)
instance Num a => Lib.Monoid (Product a) where
    mempty = Product 1

--Ord a ca semigrup fata de operatia de minim
newtype Min a = Min{getMin :: a}
    deriving (Eq, Read, Show)
instance Ord a => Lib.Semigroup (Min a) where
    Min x <> Min y = Min (min x y) 
instance (Ord a, Bounded a) => Lib.Monoid (Min a) where
    mempty = Min maxBound

--ord ca semigrup fata de operatia de maxim
newtype Max a = Max{getMax :: a}
    deriving (Eq, Read, Show)
instance Ord a => Lib.Semigroup (Max a) where
    Max x <> Max y = Max (max x y) 
instance (Ord a, Bounded a) => Lib.Monoid (Max a) where
    mempty = Max minBound

--monoidul asociat unui semigrup
instance Lib.Semigroup a => Lib.Semigroup(Maybe a) where
    Nothing <> m = m
    m <> Nothing = m
    Just m1 <> Just m2 = Just (m1 Lib.<> m2)

instance Lib.Semigroup a => Lib.Monoid (Maybe a) where
    mempty=Nothing

--monoidul endomorfismelor
newtype Endo a = Endo {appEndo :: a -> a}

instance Lib.Semigroup (Endo a) where
    Endo g <> Endo f = Endo (g . f)

instance Lib.Monoid (Endo a) where
    mempty = Endo id 
    
--tipul listelor nevide
data NonEmpty a= a :| [a]
    deriving (Eq, Ord)
infixr 5 :|

instance Lib.Semigroup (NonEmpty a) where
    (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

--concatenare pentru semigrupuri
sconcat :: Lib.Semigroup a => NonEmpty a -> a
sconcat (a :| as) = go a as
    where
        go a [] = a
        go a (b:bs) = a Lib.<> go b bs


------------------FOLDABLE-------------------------
--vrem sa generalizam fold si pe alte structuri recursive cum ar fi arborii binari

data BinaryTree a = Leaf a
                    | Node (BinaryTree a) (BinaryTree a)
    deriving Show

--foldr normal
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f i [] = i
foldr f i (x : xs) = f x (Lib.foldr f i xs)

--foldr pe binary trees
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f i (Leaf x) = f x i
foldTree f i (Node l r) = foldTree f (foldTree f i r) l

myTree = Node (Node (Leaf 1) (Leaf 2)) (Node(Leaf 3) (Leaf 4))
--apelare: foldTree (+) 0 myTree

instance Foldable BinaryTree where 
    foldr = foldTree

treel = Node ( Node ( Leaf 1 ) ( Leaf 2 ) ) ( Node ( Leaf 3 ) ( Leaf 4 ) )
treeS = Node ( Node ( Leaf "1" ) ( Leaf "2" ) )( Node ( Leaf "3" ) ( Leaf "4" ) )

--definite auromat: foldMap, foldl, maximum


----sum cu foldMap

--aveam instanta asta mai sus
-- newtype Sum a = Sum {getSum :: a}
--     deriving (Eq, Read, Show)

-- instance Num a => Lib.Semigroup (Sum a) where
--     Sum x <> Sum y = Sum (x+y)
-- instance Num a => Lib.Monoid (Sum a) where
--     mempty = Sum 0


--sum = getSum . (foldMap Sum)

-- treel=Node ( Node ( Leaf 1 ) ( Leaf 2 ) ) ( Node ( Leaf 3 ) ( Leaf 4 ) )

--apleare: foldMap Sum treel sau `sum treel`

----product cu foldMap

--product = getProduct.(foldMap Product)
--pot sa apelez direct `product treel`

--any si elem
--any = getAny . (foldMap Any)

--alem e = getAny . (foldMap (Any . (==e)))

--cum definim foldMap folosind foldr?
-- foldr :: (a -> b -> b) ->b -> t a -> b
-- foldMap :: Monoid m => (a->m) ->t a -> m

-- foldMap f tr = foldr foo i tr
--     where foo = ???
--         i = mempty


-------------------FUNCTORI----------------
class Functor m where
    fmap :: (a->b) -> m a -> m b

-- data Arbore a = Nil | (Nod a) Arbore Arbore

--instanta pentru liste
instance Lib.Functor [] where
    fmap = map

--instanta pentru tipul optiune
instance Lib.Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Lib.Functor BinaryTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node (l) (r)) = Node (Lib.fmap f l) (Lib.fmap f r)

-- instance Lib.Functor Arbore where
--     fmap f Nil = Nil
--     fmap f (Nod x l r) = Nod (f x) (Lib.fmap f l) (Lib.fmap f r)

-- instance Lib.Functor (->) a where
--     fmap f g = f . g


---------------------CATEGORII----------------------
--Categoria Set
---------------------
--obiecte: multimi
--sageti: functii
--identitati: functii identitate
--compunere: compunerea functiilor


--Categoria Hask
---------------------
--obiecte: ripuri
--sagetile: functii intre tipuri
--identitati: functia polimorfica id (id :: a- ->)
--compunere: compunerea polimorfica (.)

--Subcategorii ale lui Husk date de tipuri parametrizate
-- ex: liste, optiuni, arbori cu functii de sursa t: t -> a

--FUNCTOR: Date fiind 2 coategorii C si D, in general un functor F : C -> D
--este dat de:
--o functie F:|C| ->|D| de obiecte de la C la D
--pentru orice A,B din C, o functie F:C(A,B) -> D(F(A), F(B))
--compatibila cu identitatile si compunerea:
-----F(id_A)=id_F(A) pentru orice A
-----F(g . f)= F(g) . F(f) pentru orice f : A -> B, g : B-> C, h=g . f

--In Haskell, o instanta Functor m este data de:
--un tip m a pentru price tip a (deci m trebuie sa fie un tip parametrizat)
--pentru orice doua tipuri a si b, o functie
--fmap::(a->b)->(m a -> m b)
--compatibila cu identitatile si compunerea
--fmap id==id
--fmap (g.f)==fmap g. fmap f, pentru orice f::a->b si g::b->c

--FUNCTORI APLICATIVI
--(<*>) :: m(b -> c) -> m b -> m c

--problema:
--data fiind o functie f ::a1 -> a2 -> a3 ->... -> an-> a si computatiile
--ca1 :: m a1, .... can :: m an, vrem sa aplicam functia f pe rand computatiilor
--ca1, ... can pentru a obtine o computatie finala ca:: m a 

--stim ca: fmap :: (a->b) -> m a -> m b
-- functia (<*>) :: m(b->c) -> m b -> m c cu "cu proprietati bune"
--Atunci:
-- fmap f :: m a1 -> m (a2 -> a3 ->...->an -> a)
-- fmap f ca1 :: m (a2 -> a3 ->...->an -> a)
-- fmap f ca1 <*> ca2 :: m (a3 ->...->an -> a)
-- ...
-- fmap f ca1 <*> ca2 <*> ca3 ... <*> can :: m a

--clasa de tipuri Applicative
-- class Functor m => Applicative m where
--     pure :: a -> m a
--     (<*>) :: m (a->b) -> m a -> m b

-- !orice instanta a lui Applicative trebuie sa fie instanta a lui Functor
--pure transforma orice valoare intr-o computatie minimala care
--are acea valoare ca rezultat, si nimic mai mult
--(<*>) ia o computatie care produce functii si o computatie care produce
--argumente pentru functii si obtine o computatie care produce rezultatele
--aplicarii functiilor asupra argumentelor

--- !!! fmap f c == pure f <*> x
--- !!! (<$>) = fmap

-- --instanta pentru tipul computatiilor nedeterministe (liste)
-- instance Applicative [] where
--     pure x = [x]
--     fs <*> xs = [f x | f <- fs,  x<-xs]   

-- --instanta pentru tipul computatiilor I/O 
-- instance Applicative IO where
--     pure = return
--     iof <*> iox = do
--                     f<-iof
--                     x<-iox
--                     return (f x)

-- --tipul functiilor de sursa data
-- -- :+: is a data constructor in infix form. but what does it mean exactly..?
-- -- what does Lit mean? 
-- data Exp = Lit Int | Var String | Exp :+: Exp
-- type Env = [(String, Int)]

-- --da snd al primului element din lista env care are fst==x
-- find :: String -> (Env -> Int)
-- find x env = head [i | (y, i)<- env, y==x]

