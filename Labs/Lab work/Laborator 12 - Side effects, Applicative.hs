import qualified Data.Char     as Char
import           System.Random

-- MyRandom e un wrapper peste functii cu tipul StdGen -> (a, StdGen).
-- Practic, dam un nume tipului de functii de la StdGen la (a, StdGen),
-- cu precizarea ca MyRandom reprezinta un tip nou din perspectiva compilatorului.
-- Functiile StdGen -> (a, StdGen) sunt functii care primesc un seed (StdGen) si
-- intorc un rezultat (de tip a) si un nou seed (StdGen).
newtype MyRandom a = MyRandom { runRandom :: StdGen -> (a,StdGen) }
-- Am folosit record syntax ca sa definim tipul, ceea ce inseamna ca compilatorul
-- va genera automat o functie "getter" sau "runner" (care face unwrap):
--   runRandom :: MyRandom a -> StdGen -> (a, StdGen)
-- Dupa cum ne zice si tipul, runRandom ia un MyRandom (care stim ca reprezinta o functie),
-- un seed, si va rula functia din MyRandom cu seedul primit ca argument.
-- Remember: MyRandom din partea dreapta a egalului e constructorul de date, care
-- are tipul urmator:
--   MyRandom :: (StdGen -> (a, StdGen)) -> MyRandom a
-- Asta inseamna ca ia o functie cu tipul StdGen -> (a, StdGen) si intoarce ceva de tipul
-- MyRandom a.
--
-- Cum construim valori de tip StdGen? In System.Random avem functia mkStdGen:
--   https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html#v:mkStdGen
-- De exemplu, mkStdGen 4 ne va da un StdGen.

-- Functia randomPositive:
-- Ce inseamna definitia asta? Raspunsul sta in cine e next. Daca cautam pe Hoogle,
-- vedem ca tot in System.Random avem next :: RandomGen g => g -> (Int, g). RandomGen e o clasa
-- din System.Random. Daca ne uitam si la definitia tipului StdGen, o sa vedem ca are instanta de
-- RandomGen. Deci, daca parametrul de tip g e instantiat la StdGen, avem next :: StdGen -> (Int, StdGen).
-- Vedeti ca arata exact ca tipul din interiorul lui MyRandom, unde parametrul de tip a e instantiat la
-- tipul Int.
-- Ce anume, pentru orice tip a, ia o functie cu tipul StdGen -> (a, StdGen) si ne da un MyRandom a?
-- Raspunsul e chiar constructorul, MyRandom. In cazul nostru, 'a' e instantiat la Int, deci ne va da
-- un MyRandom Int.
-- Asta inseamna ca functia randomPositive ne va da un generator de numere intregi.
-- Atentie! Tineti minte ca sub MyRandom, e chiar o functie. Ceea ce inseamna ca daca scrieti
-- randomPositive in interpretor va da un mesaj de eroare, ca nu poate afisa lucruri de tipul MyRandom Int.
-- Nu putem nici sa scriem o instanta de Show, pentru ca, daca va mai amintiti, intr-unul din laboratoarele trecute
-- am vazut ca instantele de show pe tipuri de functii nu au tocmai sens. Puteti sa va ganditi la randomPositive ca fiind
-- un program (o computatie) pe care il putem rula cand avem nevoie de rezulatul lui.
-- Tineti minte ca avem deja functia runRandom :: MyRandom a -> StdGen -> (a, StdGen), care chiar asta face: ruleaza
-- un program de tipul MyRandom a, dandu-i un input.
-- Scrieti in interpretor runRandom randomPositive (mkStdGen 4) ca sa vedeti ca va da rezultatul rularii lui randomPositive
-- pentru cand input-ul este un seed cu valoarea 4.
randomPositive :: MyRandom Int
randomPositive = MyRandom next

-- Daca tot pare putin neclar ce se intampla aici, trebuie doar sa va ganditi ca randomPositive
-- e o functie peste care am pus un wrapper. Daca nu aveam wrapper-ul asta, puteam sa scriem direct
-- randomPositive :: StdGen -> (Int, StdGen). Am fi rulat randomPositive direct asa: randomPositive (mkStdGen 4).
-- Dar pentru ca avem wrapper-ul, trebuie sa il scoatem si runRandom ne va ajuta sa facem asta, si sa si rulam
-- functia de dedesubt.

-- De ce sa punem un wrapper peste functiile cu tipul asta?
-- Raspunsul e ca asa putem structura codul mai bine.
-- Asta se refera si la faptul ca dam un nume clar genului astuia de computatie, dar si la faptul ca
-- putem scrie instante de clase pe el.

-- Instanta Functor:
-- Asta se rezuma la definirea functiei fmap :: (a -> b) -> MyRandom a -> MyRandom b
-- Inseamna ca o sa putem aplica transformari pe rezultatul computatiilor MyRandom.
instance Functor MyRandom where
    -- fmap :: (a -> b) -> MyRandom a -> MyRandom b
    -- f :: (a -> b)
    -- (MyRandom ma) :: MyRandom a  -- (am facut match pe constructor, tineti minte ca putem face asta pe orice constructor)
    -- ma :: StdGen -> (a, StdGen)  -- observati ca sub MyRandom e o functie ma
    fmap f (MyRandom ma) =
        -- ideea aici e ca vrem sa intoarcem un MyRandom, care e o functie peste care punem constructorul MyRandom
        -- remember:
        --   cu $ aplicam MyRandom la ce e in dreapta lui $
        --   sintaxa pentru functii anonime: \arg1 arg2 ... -> corpul functiei
        -- deci, 'gen' e argumentul de tip StdGen
        MyRandom $ \gen ->
            -- ne uitam la ce avem:
            --   gen :: StdGen
            --   ma :: StdGen -> (a, StdGen)
            --   f :: a -> b
            -- pai, in primul rand putem aplica 'ma' pe 'gen', si sa extragem rezultatul
            let (result, newGen) = ma gen
            -- acum, avem si:
            --   result :: a
            --   newGen :: StdGen
            -- ce vrem sa intoarcem? seed-ul nou si rezultatul transformat cu functia f
             in (f result, newGen)
-- Puteti sa va ganditi ca aici primim doua functii, una a -> b si una care e incapsulata de MyRandom.
-- Noi vrem sa scriem o functie noua, care ruleaza cea de-a doua functie si transforma rezultatul ei
-- folosind-o pe prima. Apoi, o incapsulam si pe ea in MyRandom.

-- Functia randomBoundedInt:
-- Aici, pur si simplu folosim fmap ca sa transformam rezultatul lui randomPositive.
-- Va dati seama ca daca nu aveam fmap, trebuia sa scriem tot ce face fmap de fiecare data cand
-- voiam sa transformam rezultatul lui MyRandom intr-un fel anume.
-- Concret, randomPositive o sa genereze numere naturale random. Inainte sa il returnam, aplicam o
-- transformare pe numarul generat astfel incat sa fie mereu mai mic decat n.
randomBoundedInt :: Int -> MyRandom Int
randomBoundedInt n = fmap f randomPositive
  where
    f :: Int -> Int
    f x = x `mod` n

-- Scrieti voi randomLetter, folosind fmap si unul din generatorii
-- definiti anterior (randomBoundedInt sau randomPositive).
-- Puteti sa folositi si functii din Data.Char.
randomLetter :: MyRandom Char
randomLetter = undefined

-- Instanta Applicative (cum o folosim):
-- Applicative ne ajuta sa compunem computatii independente. Concret, o sa facem asta in felul urmator:
-- o sa luam transformari a -> b, si le incapsulam in computatia noastra. Apoi, o sa folosim o metoda prin care
-- putem sa aplicam transformari incapsulate pe alte computatii.
-- Prima parte o putem generaliza putin, in sensul ca putem defini o functie care ia ceva si intoarce acel ceva
-- ca rezultatul unui MyRandom. In Applicative asta face metoda pure, pe care trebuie sa o definim:
--   pure :: Applicative t => a -> t a   -- in cazul nostru, t va fi MyRandom
-- A doua parte e reprezentata de apply, <*>, care are tipul urmator:
--   (<*>) :: Applicative t => t (a -> b) -> t a -> t b  -- din nou, in cazul nostru t este MyRandom
-- <*> o sa ruleze prima computatie, care o sa intoarca o transformare. Apoi, o sa ruleze a doua computatie, si
-- o sa aplice transformarea pe rezultat. O sa vedeti pe un exemplu cum ne ajuta asta sa compunem computatii diferite.
--
-- (<*>) :: MyRandom (a -> b) -> MyRandom a -> MyRandom b
instance Applicative MyRandom where
    -- pure :: a -> MyRandom a
    -- cum ziceam, pure ia ceva, x :: a, si pur si simplu creeaza o computatie care returneaza acel rezultat;
    -- in cazul nostru, computatiile sunt MyRandom, care reprezinta functii StdGen -> (a, StdGen)
    -- deci intoarcem o functie care intoarce x si seed-ul primit ca argument;
    -- peste functia asta punem wrapper-ul MyRandom
    pure x = MyRandom $ \gen -> (x, gen)
    -- (<*>) :: MyRandom (a -> b) -> MyRandom a -> MyRandom b
    -- aici avem asa:
    --   mf :: StdGen -> (a -> b, StdGen)  -- deci, o computatie care intoarce o transformare a -> b
    --   ma :: StdGen -> (a, StdGen)       -- o computatie care intoarce a
    (MyRandom mf) <*> (MyRandom ma) =
        -- iarasi, pentru ca MyRandom e reprezentat de o functie, vom intoarce o functie peste care aplicam MyRandom
        MyRandom $ \gen ->
            -- din nou, gen :: StdGen
            -- ce trebuie sa facem? pai putem sa rulam mf, aplicand seed-ul primit ca argument lui mf
            let (f, newGen) = mf gen
            -- acum, vedem ca (mf gen) :: (a -> b, StdGen)
            -- deci avem o functie a -> b, pe care am denumit-o f si un nou seed, newGen
            -- dar, noi voiam sa intoarcem un rezultat de tipul b; avem a -> b acum, dar inca nu avem un a
            -- avem insa ma, pe care daca il rulam ne da un a
            -- rulam ma cu noul seed, newGen
                (a, newNewGen) = ma newGen
            -- acum avem un rezultat de tipul a, si ultimul seed generat
             in (f a, newNewGen)
            -- in final, aplicam f pe a si si intoarcem rezultatul asta impreuna cu ultimul seed generat
-- Pe scurt, extragem functia a -> b, apoi extragem a, si aplicam functia pe a. Cand extragem valorile astea,
-- tinem cont si de faptul ca aceste computatii au side effects, in cazul nostru side effect-ul e ca avem un nou
-- seed pe care trebuie sa il propagam mai departe de fiecare data.

-- Dar cum ne ajuta pure si <*> sa compunem computatii?
-- Ca sa fie usor de vazut in interpretor, o sa ne uitam mai intai la Maybe. Tineti minte ca Maybe modeleaza computatii
-- care intorc ceva (Just ceva) sau nimic (Nothing).
-- Ce se intampla daca avem o singura computatie? Sa luam o transformare, 'negate' si o computatie care intoarce un intreg, Just 3.
-- Cu pure "ridicam" transformarea 'negate' in contextul nostru, Maybe (e un mod mai sofisticat de a spune ca facem (Just negate) :) ).
-- Daca rulam in interpretor:
--   pure negate <*> Just 3
-- vedem ca rezultatul e Just (-3). Pai, a facut acelasi lucru ca fmap negate (Just 3)!
-- Asta e chiar o proprietate a lui Applicative, anume ca:
--   fmap f x = pure f <*> x
-- Dar daca avem doua computatii? Sa zicem ca vrem sa adunam rezultatul lor.
-- In primul rand, putem sa incapsulam adunarea folosind pure: pure (+). Asta va avea tipul Maybe (Int -> Int -> Int).
-- Acum, daca facem pure (+) <*> Just 3, o sa avem un rezultat Maybe (a -> a), deci putem sa il mai aplicam pe ceva!
-- Si cum mai aplicam pure (+) <*> Just 3  la ceva? Pai tot cu <*>.
-- In concluzie, urmatoarea linie de cod ne va da ce rezultatul final, Just 8:
--   pure (+) <*> Just 3 <*> Just 5
-- Puteti citi asa:
--   (pure (+) <*> Just 3) <*> Just 5
--              ^ asta ne da Just (3 +)
--              -> apoi facem Just (3 +) <*> Just 5
--              -> asta da Just 8
-- Si, scapam de error handling explicit (cand rezultatul vreunei computatii e Nothing). Rulati:
--   pure (+) <*> Nothing <*> Just 5
--
-- Functia random10LetterPair:
-- Acum, sa vedem ce se intampla pe exemplul nostru din laborator.
-- Ideea e ca vrem un generator de perechi, (numar natural < 10, litera).
-- Dar avem deja un generator pentru numere naturale mai mici ca un n dat, si un generator pentru litere.
-- Vrem sa luam rezultatele de la amandoua, si sa facem o pereche cu ele.
-- Pai, Applicative ne da un mod simplu de a face asta.
-- Luam constructorul de perechi, (,), si il aplicam mai intai lui randomBoundedInt si apoi rezultatul
-- il aplicam lui randomLetter.
random10LetterPair :: MyRandom (Int, Char)
random10LetterPair = pure (,) <*> randomBoundedInt 10 <*> randomLetter

-- Incercati voi sa faceti urmatorul exercitiu.
