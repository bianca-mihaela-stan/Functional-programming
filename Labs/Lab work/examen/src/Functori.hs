{-# LANGUAGE FlexibleInstances #-}
module Functori where
import           Data.Char      (isUpper)
import           Data.Foldable  (foldMap, foldr)
import           Data.Monoid
import           Data.Semigroup (Max (..), Min (..))


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
data Two a b = Two a b
instance Functor (Two a) where
    --fmap:: (b-> c) -> Two a b -> Two a c
    fmap f (Two x y) = Two x (f y)

----------------------------------THREE--------------------------------

--scrieti instanta de Functor pentru tipul Three de mai sus
data Three a b c = Three a b c
instance Functor (Three a b) where
    --fmap :: (c -> d) -> Three a b c -> Three a b d
    fmap f (Three x y z) = Three x y (f z)

--scrieti instanta de Functor pentru tipul Three' de mai sus

-----------------------------------THREE'-------------------------------
data Three' a b = Three' a b b
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
data Constant a b = Constant b
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
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
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
