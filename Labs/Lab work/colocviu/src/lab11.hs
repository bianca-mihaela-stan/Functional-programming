{-# LANGUAGE FlexibleInstances #-}
module Lab where
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


-------------------------EXRCITII PENTRU FUNCTOR---------------------------

--Scrieti instante ale clasei Functor pentru tipurile de date scrise mai jos.

--Legile pe care trebuie sa le verifice Functor sunt:
--fmap id == id
-- fmap (f . g)==fmai f . fmap g

-------------------------------IDENTITY-----------------------------------

newtype Identity a = Identity a

instance Functor Identity where
    --fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = Identity (f x)

--------------------------------PAIR------------------------------------

data Pair a = Pair a a

instance Functor Pair where
    --fmap :: (a->b) -> Pair a a -> Pair b b
    fmap f (Pair x y) = Pair (f x) (f y)

--scrieti instanta de Functor pentru tipul Two de mai sus

---------------------------------TWO------------------------------------

instance Functor (Two a) where
    --fmap:: (b-> c) -> Two a b -> Two a c
    fmap f (Two x y) = Two x (f y)

----------------------------------THREE--------------------------------

--scrieti instanta de Functor pentru tipul Three de mai sus
instance Functor (Three a b) where
    --fmap :: (c -> d) -> Three a b c -> Three a b d
    fmap f (Three x y z) = Three x y (f z)

--scrieti instanta de Functor pentru tipul Three' de mai sus

-----------------------------------THREE'-------------------------------

instance Functor (Three' a) where
    --fmap :: (b -> c) -> Three' a b b -> Three' a c c
    fmap f (Three' x y z) = Three' x (f y) (f z)

-----------------------------------FOUR--------------------------------

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    --fmap :: (d->e) -> Four a b c d -> Four a b c e
    fmap f (Four x y z t)= Four x y z (f t)

---------------------------------FOUR''---------------------------------

data Four'' a b = Four'' a a a b

instance Functor (Four'' a) where
    --fmap :: (b->c) -> Four'' a a b b -> Four'' a a c c
    fmap f (Four'' x y z t) = Four'' x y z (f t)

---------------------------------FOUR'''--------------------------------

data Four''' a b = Four''' a a b b

instance Functor (Four''' a) where
    --fmap :: (b->c) -> Four''' a a b b -> Four''' a a c c
    fmap f (Four''' x y z t) = Four''' x y (f z) (f t)

---------------------------------CONSTANT-----------------------------

--scrieti instanta de Functor pentru tipul Constant de mai sus
instance Functor (Constant a) where
    --fmap :: (b -> c) -> Constant a b -> Constant a c 
    fmap f (Constant y) = Constant (f y)

---------------------------------QUANT---------------------------------

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk x) = Desk x
    fmap f (Bloor y) = Bloor (f y)

--Chestia asta imi spune: este posibil sa vedem valori de tipul K a b
--care se pot obtine doar folosind constructorul K :: a -> K a b

-----------------------------------K-----------------------------------

data K a b = K a
instance Functor (K a) where
    fmap f (K a) = K a

----------------------------------FLIP--------------------------------

--Chestia asta imi spune: e posibil sa vedem valori de tipul Flip f a b
--care se pot obtine doar folosind constructorul Flip :: f b a -> Flip f a b
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
--pentru Flip nu trebuie sa faceti instanta

--Pentru ca in fmap :: (a->b) -> f a -> f b, f a are kind-ul *, ca sa scriem
--o instanca Functor pentru Flip, putem scrie doar in ultimul tip aparut
--in Flip (care este b).

--Motivul pentru care nu putem scrie ceva de genul:
    -- instance Functor (Flip f a) where
    --    fmap f (Flip x y z) = fmap Flip x y (f z)
--este pentru ca constructorul lui Flip primeste un singur argument, nu 3:
    --Flip :: f b a -> Flip f a b

--Asa ca vom scrie ceva de genul:
    -- instance Functor (Flip f a) where
    --     fmap f (Flip fxa) = undefined
--unde fxa :: f x a, iar functia f :: x -> y

--Cum am scrie functia pentru asa ceva? Incercam sa scriem pentru un f mai
--specific. Iar un astfel de f mai specific este chiar K.
-- =>
    -- instance Functor (Flip K a) where
    --     fmap f (Flip kxa) = undefined
--unde kxa :: K x a
--singurul mod sa fi obtinut o valoare de tip K x a folosind constructorul K

-- => aceasta valoare poate fi interpretata kxa = K x', unde x' :: x
--inlocuind asta in instanta noastra obtinem:
    -- instance Functor (Flip K a) where
    --     fmap f (Flip (K x')) = undefined

--Acum putem sa continuam sa scriem instanta de Functor:
--Ce facea flip al nostru? Dintr-un tip f a b obtinea Flip f b a
-- => Din tipul K x a va obtine K a x
-- Aplicand si functorul pe x => K a y
-- Deci trebuie sa obtinem o valoare de tipul Flip K a y
-- Singurul mod de a obtine o valoare de tipul Flip e folosind constructorul

-- => 
    -- instance Functor (Flip K a) where
    --     fmap f (Flip (K x')) = Flip ...
--Daca in urma aplicarii constructorului se obtine Flip K a y, 
--atunci tipul valorii initiale este K y a.
--Iar singurul mod de a obtine un element de tip K y a e prin constructorul 
--lui K.
    -- instance Functor (Flip K a) where
    --     fmap f (Flip (K x')) = Flip (K ...)

--Constructorul care retuneaza ceva de tipul K y a primeste ceva de tipul y.
--Avem o functie x' :: x si o functie f :: x -> y. Deeeci
    
instance Functor (Flip K a) where
    --fmap:: x -> y
    fmap f (Flip (K x')) = Flip (K (f x'))

--s-ar putea sa fie nevoie sa adaugati unele constrangeri la definirea instantelor


-------------------------------LIFT IT OUT------------------------------

data LiftItOut f a = LiftItOut (f a)

--Idee initiala:
--Functorul se aplica pe ultimul tip din scrierea lui LiftItOut, deci pe a
--Deci f e "constanta".
--Si vreau ca prin functor sa schimb tipul a intr-un tip b.

-- instance Functor (LiftItOut f) where
--     --funct :: a -> b
--     --fa :: f a
--     --fmap :: (a -> b) -> LiftItOut (f a) -> LiftItOut (f b)
--     fmap funct (LiftItOut fa) = LiftItOut(...)

--Ce as vrea acum sa fac la (...)?
--Pai as vrea ca in loc de fa sa am fb :: f b. Cum schimb tipul pe care se
--aplica? Cu un functor!
--Ce inseamna asta? Ca si f trebuie sa fie Functor.
--Daca si f e functor, inseamna ca pot sa aplic fmap pe f.
--Acel fmap ar arata ceva de genul:
--fmap :: (a -> b) -> f a -> f b

-- => ca sa il transform pe fa:: f a in fb :: f b
--nu trebuie decat sa scriu fmap funct fa

instance (Functor f) =>Functor (LiftItOut f) where
    --funct :: a -> b
    --fa :: f a
    --fmap :: (a -> b) -> LiftItOut (f a) -> LiftItOut (f b)
    fmap funct (LiftItOut fa) = LiftItOut(fmap funct fa)


-------------------------------PARAPPA-------------------------------

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    --fmap::(a -> b) -> DaWrappa (f a) (g a) -> DaWrappa (f b) (g b)
    fmap funct (DaWrappa fa ga) = DaWrappa (fmap funct fa) (fmap funct ga)

------------------------------IGNORE ONE-------------------------------

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
    --fmap :: (b -> c) -> IgnoringSomething (f a) (g b) -> IgnoringSomething (f a) (g c)
    fmap funct (IgnoringSomething fa gb) = IgnoringSomething fa (fmap funct gb)

-------------------------------NOTORIOUS--------------------------------

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    --fmap :: (t -> x) -> Notorious (g o) (g a) (g t) -> Notorious (g o) (g a) (g x)
    fmap funct (Notorious go ga gt) = Notorious go ga (fmap funct gt)

--------------------------------GOATLORD-------------------------------
--data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor (GoatLord) where
    --fmap :: (a -> b) -> No
    fmap _ NoGoat = NoGoat
    fmap funct (OneGoat a)= OneGoat (funct a)
    fmap funct (MoreGoats x y z) = 
        MoreGoats (fmap funct x) (fmap funct y) (fmap funct z)

------------------------------TALK TO ME---------------------------------

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor (TalkToMe) where
    fmap _ Halt = Halt
    fmap funct (Print str a) = Print str (funct a)
    fmap funct (Read stra ) = Read (fmap funct stra)
--- <=>                            (funct . stra)
------------------------ !pentru ca pentru functii fmap == .
