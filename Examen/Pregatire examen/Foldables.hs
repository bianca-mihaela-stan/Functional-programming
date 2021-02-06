{-# LANGUAGE FlexibleInstances #-}
module Foldables where
import           Data.Char      (isUpper)
import           Data.Foldable  (foldMap, foldr)
import           Data.Monoid
import           Data.Semigroup (Max (..), Min (..))

----------------------------EXERCITIUL 1--------------------------------

--Implementati urmatoarele functii folosind foldMap si/sau foldr din 
--clasa Foldable, apoi testati-le cu mai multe tipuri care au instanta 
--pentru Foldable.


-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
--   - (a -> m) e o functie care ia un element de tip a si il transforma
--   intr-un element de tip m; tipul m are instanta de Monoid
--   - (t a) e o structura care contine elemente de tip a
--   - foldMap transforma fiecare element din structura cu functia data,
--   si le combina pe toate intr-un singur element de tip m folosind instanta
--   de Monoid de pe m
--   - poate fi scris cu foldr:
--       foldMap f ta = foldr (mappend . f) mempty ta

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--   - am folosit foldr pe liste pana acum, aceeasi idee se aplica si la
--   alte structuri: elementele structurii sunt combinate folosind o operatie
--   asociativa la dreapta
--   - intuitiv, poate fi vazut ca o varianta mai generala a lui foldMap,
--   pentru ca tipul b nu e constrans sa fie Monoid, dar inseamna ca
--   trebuie sa ii zicem noi cum sa combine toate elementele de tip b, si
--   folosim direct operatia de combinare (a -> b -> b) pe toate elementele structurii,
--   plecand de la un element de tip b; la foldMap, pentru ca m e Monoid,
--   operatia de combinare si elementul de plecare sunt deja definite: (<>) si mempty


-----------------------------ELEM--------------------------------

-- =>
--x e un element pe care putem face operatia de egalitate
--collection este un foldable cu egalitate
--aplic functia isAnyEqual pe fiecare element din collection
--si apoi le unesc pe toate prin modalitatea specificata de Foldable 
--pentru collection
elem:: (Foldable t, Eq a) => a -> t a -> Bool 
elem x collection = getAny $ foldMap isAnyEqual collection
    where 
        --isAnyEqual :: a -> Any -- daca decomentez asta nu merge, dar nu stiu exact de ce?
        isAnyEqual y = Any (x == y)

--cum ruleaza chestia asta:
--    elem 2 [3,2,5]
-- => [Any False, Any True, Any False]
-- => Any False <> Any True <> Any False
-- => Any True
-- => True

-------------------------------NULL------------------------------

-- foldr e apelat cu o functie de agregare si un element de plecare
-- Aici putem folosi foldr cu rationamentul urmator:
--   - foldr mereu asociaza elementul de plecare cu "starea vida" a structurii,
--   in sensul ca daca nu are pe ce sa aplice functia de agregare o sa returneze
--   elementul de plecare, deci el trebuie sa fie True
--   - daca foldr are pe ce sa aplice functia de agregare, vrem sa intoarcem False

--foldr ia o functie cu 2 argumente(hence why \_ _ -- lambda function cu 2 argumente)
--daca foldr ajunge sa fie apelat pe ceva, inseamna ca colectia nu e null
--altfel e null => True
null :: (Foldable t) => t a -> Bool
null collection = foldr (\_ _-> False) True collection


--------------------------------LENGTH-------------------------------------

-- Vedeti definitia lui Sum in linkul cu Data.Monoid de mai sus.
-- Inlocuim fiecare element cu 1, si aplicam constructorul Sum peste.
-- foldMap o sa combine toate elementele din structura folosind instanta de monoid a lui Sum,
-- deci o sa faca suma.
-- Ati mai vazut functia const pana acum? Tipul ei e:
--   const :: a -> b -> a
-- Pentru orice argument de tip b o sa intoarca mereu argumentul de tip a.
length :: (Foldable t) => t a -> Int
length = getSum . foldMap (Sum . const 1)

--varianta in care am parametru
--transforma fiecare element din x intr-un Sum 1 (de tip foldable Sum) 
--si apoi foldMap combina aceste elemente dupa cum ii spune Sum, adica face suma 
length' :: (Foldable t) => t a -> Int
length' x = getSum ( foldMap (\y -> Sum 1) x )

length'' :: (Foldable t, Num a1) => t a2 -> Sum a1
length'' x = foldMap (\y -> Sum 1) x 

------------------------------TO LIST----------------------------------

-- Aici combinam elementele structurii folosind (:) si []. Nu are cum sa dea
-- altceva decat o lista cu elementele respective :)
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-------------------------------FOLD-------------------------------------

--fold ar trebui sa combine elementele unei structuri folosind 
--structura de monoid a acestora

fold :: (Foldable t, Monoid m) => t m -> m
fold x = foldMap id x
--fold [[1,2],[3,4]] => [1,2,3,4]


-----------------------------EXERCITIUL 2-----------------------------
--Scrieti instante ale lui Foldable pentru urmatoarele tipuri:

-----------------------------CONSTANT--------------------------------

data Constant a b = Constant b

-- Trebuie sa va amintiti ce am discutat la laboratorul 9 despre kinds. Foldable asteapta un constructor de tip
-- cu kind-ul * -> *. Scrieti urmatoarea comanda in interpretor ca sa vedeti ce kind are Constant:
--   :k Constant
-- Daca fixam primul tip pe care il asteapta Constant, sa zicem Constant Int (putem folosi orice tip cu kind *),
-- care va fi kind-ul acum?
--   :k Constant Int
-- Deci trebuie fixat primul tip ca sa avem kind-ul potrivit, si pentru ca vrem ca instanta de Foldable sa fie pentru orice
-- tip care apare ca prim argument la Constant, putem sa folosim un parametru de tip, a.
-- Asta mai inseamna ca o sa consideram ca elementele structurii sunt de tipul b, mai exact, de cel mai "din dreapta" parametru
-- de tip din definitia constructorului de tip. In cazul nostru am definit data Constant a b = ..., deci elementele vor fi
-- de tip b. Vedeti ca am scris mai jos tipul lui foldMap, din care reiese ca noi consideram practic doar elementele de tip b,
-- ele vor fi agregate si combinate in functie de implementarea din instanta Foldable.
instance Foldable (Constant a) where
    -- foldMap :: (b -> m) -> Constant a b -> m
    -- Avem f :: (b -> m) si x :: b din (Constant x), deci aplicam f pe x ca sa ne dea ceva de tip m.
    -- Cum putem intelege implementarea: Constant e o structura care contine un singur element,
    -- deci nu avem ce combina, e suficient doar sa scoatem elementul din structura si sa aplicam functia f.
    foldMap f (Constant x) = f x

------------------------------TWO---------------------------------

data Two a b = Two a b

-- Asemanator cu Constant, consideram ca elementele structurii sunt cele de tip b. Scrieti voi tipul lui foldMap ca sa va
-- convingeti. Deci nu putem sa combinam pe x cu y, pentru ca, x de fapt face parte din structura (structura Foldable este
-- de tipul (Two a), iar x e de tipul a). Asadar, doar aplicam f pe y ca la Constant, si il ignoram pe x.
instance Foldable (Two a) where
    --foldMap :: ( b -> m) -> Two a b -> m
    foldMap f (Two x y) = f y

-------------------------------THREE-----------------------------------

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three x y z) = f z

------------------------------THREE'-----------------------------------

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap f (Three' x y z) = f y <> f z

----------------------------FOUR'-------------------------------------

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' x y z t) = f y <> f z <> f t

--------------------------------GOATLORD--------------------------------

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Foldable GoatLord where
    -- cand nu avem capre, nu avem pe ce aplica f deci intoarcem direct elementul neutru
    foldMap f NoGoat = mempty
    -- putem aplica f pe capra, si intoarcem rezultatul
    foldMap f (OneGoat x) = f x
    -- vrem sa apelam foldMap recursiv pe toate cele 3 branch-uri de capre, si putem sa combinam rezultatele
    -- folosind operatia de monoid
    foldMap f (MoreGoats g1 g2 g3) =
        let res1 = foldMap f g1
            res2 = foldMap f g2
            res3 = foldMap f g3
         in res1 <> res2 <> res3

---------------------------------EXERCITIUL 3------------------------------
--Scrieti o functie de filtrare pentru Foldable
--filterF primeste o functie funct :: a -> Bool, unde a e un monoid
--si mai primeste un foldable monoid u(deci o structura care are definita o 
--operatie asociativa cu element neutru)
--foldMap aplica (\x -> if f x then pure x else mempty) pe fiecare element din u
--deci, daca isUpper=True, il ridicam pe x rangul de f (Applicatve)
--altfel, acesta este elementul neutru de la monoid
--f e si el Monoid, deci se face "combinarea" elementelor intr-un singur element
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a-> Bool) -> t a -> f a
filterF funct u= foldMap (\x -> if funct x then pure x else mempty) u
--care sa verifice testele de mai jos:
unit_testFilterF1=filterF Data.Char.isUpper"aNA aRe mEre"=="NARE"
unit_testFilterF2=filterF Data.Char.isUpper"aNA aRe mEre"==First(Just 'N')
unit_testFilterF3=filterF Data.Char.isUpper"aNA aRe mEre"==Min 'A'
unit_testFilterF4=filterF Data.Char.isUpper"aNA aRe mEre"==Max 'R'
unit_testFilterF5=filterF Data.Char.isUpper"aNA aRe mEre"==Last(Just 'E')

--De ce pot sa am acelasi LHS si diferite RHS? 
--Pentru ca RHS ii da compilatorului constrngerea dupa care alege
--tipul de Applicative f
unit_testFilterF6=filterF Data.Char.isUpper"aNA aRe mEre" :: First Char

-- Incercati voi sa implementati filterF. Uitati-va la constrangeri:
--   - f e Applicative: asta inseamna ca putem lua un element si il putem baga in structura f,
--   folosind pure :: Applicative f => a -> f a
--   - (f a) e Monoid: avem un element neutru la operatia de combinare pe care vrem sa o aplicam elementelor
--   structurii t
--   - t e Foldable, deci putem folosi foldMap
-- Primul argument e un predicat, putem folosi predicatul ca sa definim o functie care dupa raspunsul lui
-- pastreaza elementul sau il inlocuieste cu ceva care va fi ignorat cand combinam elementele structurii cu operatia de monoid.

