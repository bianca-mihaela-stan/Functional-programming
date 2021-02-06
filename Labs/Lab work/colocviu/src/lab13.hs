import Data.Char
import Data.List
import Control.Monad ( replicateM )
----------------------------EXEMPLUL 1-----------------------------------
--Citeste de la tastatura un sir si afiseaza rezultatul prelucrat.

prelStr :: [Char] -> [Char]
prelStr strin = map toUpper strin
ioString :: IO ()
ioString = do
           strin <- getLine 
           putStrLn ("Intrare\n" ++ strin)
           let strout = prelStr strin
           putStrLn ("Iesire\n"++strout)

ioString' :: IO ()
ioString' =
    getLine >>= (\strin -> putStrLn ("Iesire\n" ++ strin)
    >> let strout = prelStr strin
    in putStrLn ("Iesire\n"++ strout)
    ) 

-----------------------------EXEMPLUL 2---------------------------------
--Citeste de la tastatura un numar si afiseaza rezultatul prelucrarii.

prelNo :: Floating a => a -> a
prelNo noin= sqrt noin
ioNumber :: IO ()
ioNumber = do
           noin <-readLn :: IO Double
           putStrLn ("Intrare\n" ++ (show noin))
           let noout = prelNo noin
           putStrLn ("Iesire")
           print noout

-----------------------------EXEMPLUL 3----------------------------------
--Citirea din fisier de instrare si afisarea rezultatului in fisier de iesire.

inoutFile :: IO ()
inoutFile = do
            sin <- readFile "Input.txt"
            putStrLn ("Intrare\n"++sin)
            let sout = prelStr sin
            putStrLn ("Iesire\n"++sout)
            writeFile "Output.txt" sout

------------------------------EXERCITIUL 1-------------------------------
readPerson :: IO (String, Int)
readPerson = do
    nume <- getLine
    varsta <- readLn
    return (nume, varsta)

showPerson :: (String, Int) -> String
showPerson (nume, varsta) = nume <> " (" <> show varsta <> " ani)"

showPersons :: [(String, Int)] -> String
showPersons [] = ""
showPersons [p] = "Cel mai in varsta este " <> showPerson p <> "."
showPersons ps =
    "Cei mai in varsta sunt: "
    <> intercalate ", " (map showPerson ps)
    <> "."

ceiMaiInVarsta :: [(a, Int)] -> [(a, Int)]
ceiMaiInVarsta ps = filter ((== m) . snd) ps
  where
    m = maximum (map snd ps)

ex1 = do
    n <- readLn :: IO Int
    persons <- sequence (replicate n readPerson)
    let ceiMai = ceiMaiInVarsta persons
    putStrLn (showPersons ceiMai)

------------------------------PROPRIA NOASTRA MONADA----------------------
type Input = String 
type Output = String

newtype MyIO a = MyIO { runIO :: Input -> (a, Input, Output)}
--compilatorul genereaza urmatoarele functii:
--   - MyIO :: (Input -> (a, Input, Output)) -> MyIO a  (constructorul)
--   - runIO :: MyIO a -> Input -> (a, Input, Output)

------------------------------MY GET CHAR--------------------------------

myGetChar :: MyIO Char 
myGetChar = MyIO f
    where
        f::Input -> (Char, Input, Output)
        f input = 
            case input of
                [] -> ('\0', [], []) -- sau empty input
                (c :cs) -> (c, cs, [])

testMyGetChar :: Bool 
testMyGetChar = runIO myGetChar "Ana" == ('A', "na", "")

-------------------------------MY PUT CHAR---------------------------------
myPutChar :: Char -> MyIO ()
myPutChar c = MyIO f
    where
        f :: Input -> ((), Input, Output)
        f input = ((), input, [c])

testMyPutChar :: Bool 
testMyPutChar = runIO (myPutChar 'C') "Ana" == ((), "Ana", "C")

-------------------------------MY IO CA INSTANTA FUNCTOR------------------
-- fmap :: Functor m => (a -> b) -> m a -> m b
instance Functor MyIO where
    fmap f (MyIO mIO) = MyIO g
      where
        -- f :: a -> b
        -- mIO :: Input -> (a, Input, Output)
        -- input :: Input
        -- g :: Input -> (b, Input, Output)
        g input =
            let (result, newInput, output) = mIO input
             in (f result, newInput, output)

testFunctorMyIO :: Bool 
testFunctorMyIO = runIO (fmap toUpper myGetChar) "ana" == ('A', "na", "")

-----------------------------MY IO CA INSTANTA APPLICATIVE---------------

instance Applicative MyIO where
  pure a = MyIO (\sin -> (a, sin, ""))
  iof <*> ioa = MyIO iob
    where
      iob sin = (f a, sin'', sout' ++ sout'')
        where
          (f, sin', sout') = runIO iof sin
          (a, sin'', sout'') = runIO ioa sin'
 
testPureMyIO :: Bool
testPureMyIO = runIO (pure 'C') "Ana" == ('C', "Ana", "")
 
testApMyIO :: Bool
testApMyIO = runIO (pure (<) <*> myGetChar <*> myGetChar) "Ana" == (True, "a", "")

testApMyIO' :: Bool
testApMyIO' = runIO (myGetChar <* myPutChar 'E' <* myPutChar 'u') "Ana" == ('A', "na", "Eu")

-------------------------------MY IO CA INSTANTA MONADA-------------------

-- Acum, am ajuns la implementarea instantei de Monad.
-- O sa vedeti ca de multe ori apare si functia return in definitia lui Monad:
--   return :: Monad m => a -> m a
-- Mereu avem ca return = pure, de la instanta de Applicative. Motivul pentru care exista si pure si return e
-- ca Applicative a aparut dupa Monad.
-- Cum spuneam mai sus, trebuie sa definim (>>=) care ia o computatie, o ruleaza, si ii da rezultatul functiei
-- primita ca argument. In plus, avem grija ca efectele descrise de computatie sa se compuna cum ne asteptam.
-- Concret, asta inseamna ca pentru MyIO vrem sa luam rezulatul primei computatii, si sa rulam functia pe el.
-- Apoi, pastram ultimul input si intoarcem outputul vechi impreuna cu cel nou.
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
instance Monad MyIO where
    return = pure
    -- (>>=) :: MyIO a -> (a -> MyIO b) -> MyIO b
    MyIO fa >>= aToMb = MyIO fb
      where
        -- fa :: Input -> (a, Input, Output)
        -- input :: Input
        -- aToMb :: a -> MyIO b
        -- fb :: Input -> (b, Input, Output)
        fb input =
            let (result, newInput, output) = fa input -- fa input ne da un rezultat, un nou input si un output
                (newResult, newNewInput, newOutput) = runIO (aToMb result) newInput -- luam rezulatul, i-l dam lui aToMb si rulam computatia cu noul input
             in (newResult, newNewInput, output ++ newOutput) -- intoarcem rezulatul nou, ultimul input si toate output-urile
-- Cred ca acum se vede clar cum noua computatie depinde de rezultatul computatiei primita ca argument.

-- Iar, rulati testul si incercati sa il intelegeti.
testBindMyIO :: Bool
testBindMyIO = runIO (myGetChar >>= myPutChar) "Ana" == ((), "na", "A")

-- Cum ziceam, daca avem instanta de Monad atunci avem si do-notation! Deci putem sa scriem functii cu do-notation pentru orice
-- tip definit de noi, atata timp cat tipul acela are instanta de Monad.
procMyIO :: MyIO ()
procMyIO = do
    c <- myGetChar
    myPutChar (toUpper c)
    myPutChar (chr (ord c + 15))
-- Incercati runIO procMyIO "abc" in interpretor.

-- Ce face functia de mai jos?
recursiveProcMyIO :: MyIO ()
recursiveProcMyIO = do
    c <- myGetChar
    if c == '\0'
        then return ()
        else do
            myPutChar (toUpper c)
            recursiveProcMyIO
-- Rulati cu runIO recursiveProcMyIO "abc" in interpretor.