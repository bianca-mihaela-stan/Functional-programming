{-# LANGUAGE FlexibleInstances #-}
import           Data.Char      (isUpper)
import           Data.Foldable  (foldMap, foldr)
import           Data.Monoid
import           Data.Semigroup (Max (..), Min (..))

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
--   - (a -> m) e o functie care ia un element de tip a si il transforma
--   intr-un element de tip m; tipul m are instanta de Monoid
--   - (t a) e o structura care contine elemente de tip a
--   - foldMap transforma fiecare element din structura cu functia data,
--   si le combina pe toate intr-un singur element de tip m folosind instanta
--   de Monoid de pe m
--   - poate fi scris cu foldr:
--       foldMap f ta = foldr (mappend . f) mempty ta
--
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
--

-- Peste unele tipuri se pot defini mai multe instante de Monoid/Semigroup.
-- Spre exemplu:
--   - pe Int avem si monoid cu adunarea dar si monoid cu inmultirea
--   - pe Bool avem monoid cu disjunctia dar si monoid cu conjunctia
-- In Haskell nu putem defini doua instante diferite de aceeasi clasa pe un tip
-- (de ex. doua instante diferite de Monoid pe Bool). Compilatorul nu ar sti pe care
-- sa o aleaga.
-- O solutie este sa definim newtype-uri peste astfel de tipuri, si sa scriem cate o instanta
-- pe fiecare newtype. De ex. pentru Bool avem newtype-urile All si Any pentru care au fost
-- implementate cele doua instante diferite de monoid.
-- Vedeti https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html

-- newtype Any = Any { getAny :: Bool }
-- Instanta de Semigroup se comporta ca disjunctia logica:
--   Any b1 <> Any b2 == Any (b1 || b2)
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x collection =
    getAny
    $ foldMap isAnyEqual collection
  where
    -- isAnyEqual :: a -> Any
    isAnyEqual y =
        Any (x == y)

--    elem 2 [3,2,5]
-- => [Any False, Any True, Any False]
-- => Any False <> Any True <> Any False
-- => Any True
-- => True

-- foldr e apelat cu o functie de agregare si un element de plecare
-- Aici putem folosi foldr cu rationamentul urmator:
--   - foldr mereu asociaza elementul de plecare cu "starea vida" a structurii,
--   in sensul ca daca nu are pe ce sa aplice functia de agregare o sa returneze
--   elementul de plecare, deci el trebuie sa fie True
--   - daca foldr are pe ce sa aplice functia de agregare, vrem sa intoarcem False
null :: (Foldable t) => t a -> Bool
null collection = foldr (\_ _ -> False) True collection

-- Vedeti definitia lui Sum in linkul cu Data.Monoid de mai sus.
-- Inlocuim fiecare element cu 1, si aplicam constructorul Sum peste.
-- foldMap o sa combine toate elementele din structura folosind instanta de monoid a lui Sum,
-- deci o sa faca suma.
-- Ati mai vazut functia const pana acum? Tipul ei e:
--   const :: a -> b -> a
-- Pentru orice argument de tip b o sa intoarca mereu argumentul de tip a.
length :: (Foldable t) => t a -> Int
length = getSum . foldMap (Sum . const 1)

-- Aici combinam elementele structurii folosind (:) si []. Nu are cum sa dea
-- altceva decat o lista cu elementele respective :)
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- fold e foldMap unde nu mai e nevoie sa facem "map" (sa transformam elementele in elementele unui monoid).
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- Ca sa intelegeti implementarile urmatoare, e foarte important sa va uitati la tipuri.

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

data Two a b = Two a b

-- Asemanator cu Constant, consideram ca elementele structurii sunt cele de tip b. Scrieti voi tipul lui foldMap ca sa va
-- convingeti. Deci nu putem sa combinam pe x cu y, pentru ca, x de fapt face parte din structura (structura Foldable este
-- de tipul (Two a), iar x e de tipul a). Asadar, doar aplicam f pe y ca la Constant, si il ignoram pe x.
instance Foldable (Two a) where
    foldMap f (Two x y) = f y

-- Scrieti voi instanta asta, e ca la Two dar va trebui sa fixati mai multi parametri de tip ca sa fie kind-ul potrivit.
data Three a b c = Three a b c

data Three' a b = Three' a b b

-- Diferenta intre Three si Three' poate parea mica, dar e foarte importanta. Acum, avem doua elemente de tip b,
-- deci doar elementul de tip a face parte din structura, iar cele doua elemente de tip b sunt elementele structurii.
-- In implementare, aplicam f pe cele doua elemente si le combinam folosind operatia de monoid.
instance Foldable (Three' a) where
    foldMap f (Three' x y z) = f y <> f z

-- Scrieti voi instanta asta, e foarte asemanatoare cu Three'.
data Four' a b = Four' a b b b

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
-- hint: folositi foldMap

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

-- Incercati voi sa implementati filterF. Uitati-va la constrangeri:
--   - f e Applicative: asta inseamna ca putem lua un element si il putem baga in structura f,
--   folosind pure :: Applicative f => a -> f a
--   - (f a) e Monoid: avem un element neutru la operatia de combinare pe care vrem sa o aplicam elementelor
--   structurii t
--   - t e Foldable, deci putem folosi foldMap
-- Primul argument e un predicat, putem folosi predicatul ca sa definim o functie care dupa raspunsul lui
-- pastreaza elementul sau il inlocuieste cu ceva care va fi ignorat cand combinam elementele structurii cu operatia de monoid.
filterF
     :: ( Applicative f
        , Foldable t
        , Monoid (f a)
        )
     => (a -> Bool) -> t a -> f a
filterF = undefined -- Hint folosiți foldMap si pure

unit_testFilterF1 = filterF Data.Char.isUpper "aNA aRe mEre" == "NARE"
unit_testFilterF2 = filterF Data.Char.isUpper "aNA aRe mEre" == First (Just 'N')
unit_testFilterF3 = filterF Data.Char.isUpper "aNA aRe mEre" == Min 'A'
unit_testFilterF4 = filterF Data.Char.isUpper "aNA aRe mEre" == Max 'R'
unit_testFilterF5 = filterF Data.Char.isUpper "aNA aRe mEre" == Last (Just 'E')

-- fmap :: Functor t => (a -> b) -> t a -> t b
--
-- Seamana cu functia map de la liste: map :: (a -> b) -> [a] -> [b],
-- care aplica o functie pe toate elementele listei.
-- De fapt, implementarea lui fmap pentru liste e chiar map!
--
-- Intuitiv:
--   - spre deosebire de Foldable unde cam toate tipurile pe care se pot defini
--   instante de Foldable reprezinta colectii de elemente, tipurile
--   pe care putem defini instante de Functor pot fi si actiuni/computatii
--   (cred ca tipurile de genul asta cam toate sunt la baza functii/pot fi reprezentate ca functii)
--   - deci putem citi (t a) ca fiind o computatie care intoarce a, si folosind fmap putem sa
--   facem ca, computatia t sa intoarca b
--   - sau putem citi ca fmap :: (a -> b) -> (t a -> t b) (tineti minte ca (->) e asociativ la dreapta),
--   in sensul ca ridicam transformarea (a -> b) in contextul t, deci o sa avem o transformare (t a -> t b)
--   - sau daca tipul t e de fapt o colectie (o lista, un arbore etc.), putem citi ca fmap o sa transforme
--   fiecare element din colectia respectiva
--
-- E important de mentionat ca fmap trebuie sa pastreze structura lui t. Formal, asa cum la Semigroup si Monoid
-- aveam niste legi (asociativitate si element neutru) care trebuiau sa fie adevarate pentru orice instanta,
-- la fel avem niste legi pentru Functor. Daca cautati Functor pe hoogle vedeti in documentatie care sunt legile,
-- incercati sa va ganditi ce inseamna si de ce "pastreaza structura".

newtype Identity a = Identity a

instance Functor Identity where
    -- fmap :: (a -> b) -> Identity a -> Identity b
    -- Pur si simplu aplicam f pe elementul din Identity.
    fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a

instance Functor Pair where
    -- fmap :: (a -> b) -> Pair a a -> Pair b b
    -- Pur si simplu aplicam f pe elementele din Pair.
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

-- scrieți instanță de Functor pentru tipul Two de mai sus
--
-- Aici, trebuie sa tinem cont de faptul ca iar, Functor cere un tip cu kind-ul * -> *. Din acelasi rationament ca
-- la Foldable reiese ca Two va fi Functor in cel mai din dreapta argument, b, iar a va face parte din structura.
instance Functor (Two a) where
    -- fmap :: (b -> c) -> Two a b -> Two a c
    -- Pentru ca am denumit cel mai din dreapta argument al lui Two, b, trebuie sa redenumim putin parametrii de tip,
    -- sper sa nu va incurce asta.
    -- Deci transformam doar elementul de tipul in care Two e Functor.
    fmap f (Two a b) = Two a (f b)

-- scrieți instanță de Functor pentru tipul Three de mai sus
-- Incercati sa scrieti voi instanta asta. Vedeti hint-ul de la Foldable.

-- scrieți instanță de Functor pentru tipul Three' de mai sus
-- Incercati sa scrieti voi instanta asta. Uitati-va la instanta de Foldable a lui Three',
-- si la instanta de Functor a lui Pair.

data Four a b c d = Four a b c d
-- Incercati sa scrieti si instanta asta. Seamana cu Two, doar ca trebuie fixati mai multi parametri.

data Four'' a b = Four'' a a a b
-- Am scris eu instanta asta, ca sa nu va incurcati, si sa vedeti ca nu conteaza ca avem multe elemente de tip a,
-- Four'' este Functor doar in tipul care apare cel mai in dreapta din definitia constructorului de tip.
-- In caz ca ati uitat:
--   - definitia constructorului de tip e ce apare in stanga egalului, deci:
--       data Four'' a b
--   - in dreapta egalului e constructorul de date, care nu ne intereseaza in contextul asta
instance Functor (Four'' a) where
    fmap f (Four'' a1 a2 a3 b) = Four'' a1 a2 a3 (f b)

-- scrieți o instanță de Functor penru tipul Constant de mai sus
-- Incercati sa scrieti si instanta asta.

data Quant a b = Finance | Desk a | Bloor b
-- Aici avem un sum type, acelasi rationament se aplica si aici. Va fi functor doar in tipul b.
instance Functor (Quant a) where
    -- nu avem pe ce sa aplicam functia deci intoarcem tot Finance
    fmap _ Finance   = Finance
    -- cum ziceam, e functor in b nu in a, deci nu se poate aplica functia f aici
    fmap f (Desk a)  = Desk a
    -- aici putem aplica functia f
    fmap f (Bloor b) = Bloor (f b)

data K a b = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
  -- pentru Flip nu trebuie să faceți instanță

-- Aici definim instanta de Functor pe un tip mai putin general, adica primul tip pe care il dam
-- constructorului de tip Flip nu mai e un parametru ci tipul K. Incercati sa intelegeti implementarea
-- uitandu-va la tipuri.
instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip (K (f b))

-- Daca ne uitam la ce e in dreapta egalului, vedem ca f e aplicat lui a. Asta inseamna ca f e un constructor de tip
-- care asteapta un argument, cu alte cuvinte, are kind-ul * -> *. Ca sa putem sa definim o instanta de Functor pe
-- LiftItOut, avem nevoie de informatie in plus despre f, mai exact, ca f e si el functor.
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    -- fa :: f a, deci facem fmap f peste el si aplicam deasupra rezultatului constructorul de date LiftItOut
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

-- Acum, observam acelasi lucru ca mai sus pentru doua din argumentele constructorului de tip Parappa. Si f si g
-- sunt constructori de tip care asteapta un argument. Ca Parappa sa fie functor in argumentul cel mai din dreapta, a,
-- avem nevoie de constrangeri pe ambele tipuri f si g. Peste ambele trebuie sa putem sa facem fmap.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- Asta seamana cu Parappa, dar mai are un argument de tip. Si in plus, doar unul din elemente e "ceva care contine/returneaza"
-- ultimul argument din dreapta, adica b. Deci avem nevoie de constrangere doar pentru g.
-- Observati ca pentru ca in dreapta egalului nu aplicam a si b altui tip, ele au kind-ul *.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- Aici avem un singur argument de kind * -> *, si stim ca Notorious va fi Functor doar in cel mai din dreapta argument, t.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- scrieți o instanță de Functor pentru tipul GoatLord de mai sus
-- Incercati voi sa scrieti instanta asta.

-- Aici avem un exemplu clar cand definim o instanta de Functor peste un tip care reprezinta o actiune.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt          = Halt
    fmap f (Print str a) = Print str (f a)
    fmap f (Read g)      = Read (f . g) -- care e echivalent cu Read (fmap f g), tineti minte ca pentru functii fmap == .
