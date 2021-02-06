import           Data.Char


-- Tipul IO:
-- IO este un tip special, in sensul ca e singurul tip care ne lasa sa facem operatii
-- care interactioneaza cu lumea exterioara. Putem sa citim de la stdin, sa afisam la
-- stdout/stderr, sa interactionam cu fisiere, cu alte procese, cu memoria, cu tot ce ne pune la dispozitie
-- sistemul de operare. Ce putem sa facem in alte limbaje de programare din start, fara sa ne impuna compilatorul
-- vreo restrictie, in Haskell putem sa facem doar in IO. Poate parea foarte restrictiv, dar ne forteaza
-- sa ne gandim explicit care parti din programul nostru trebuie sa poata sa faca astfel de interactiuni
-- cu lumea exterioara, si sa le separam de partile din program care nu au nevoie de asa ceva.
-- Asta e un good practice indiferent de limbajul de programare folosit.
--
-- Clasa Monad (introducere, si cum o folosim):
-- E natural ca operatiile facute in IO trebuie sa poata sa fie secventiate. Adica, sa zicem ca avem o computatie
-- IO care citeste un fisier si inca o computatie IO care afiseaza la stdout. Vrem sa putem sa citim fisierul mai intai,
-- apoi sa luam rezultatul citirii fisierului si sa il afisam la stdout. Deci computatia care afiseaza la stdout va depinde
-- de rezultatul dat de computatia care citeste din fisier. In plus, putem sa facem asta si cu alte tipuri de computatii,
-- nu doar cu IO.
-- Deci chiar am putea generaliza ideea asta de a secventia computatii.
-- In Haskell, generalizarea acestei idei e data de clasa Monad. Deci orice tip care are instanta de monad are urmatoarea metoda:
--   (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- Functia asta se numeste bind, si o putem intelege in felul urmator:
--   bind o sa ruleze m a, apoi o sa aplice functia (a -> m b) primita ca argument pe rezultatul dat de m a,
--   deci, o sa construiasca o computatie care intoarce un rezultat de tip b.
-- Asa vedem ca folosind instanta de Monad, putem compune computatii care depind unele de celelalte.
-- Sa luam un exemplu:
--   Sa zicem ca vrem sa citim o linie de la stdin, apoi sa prelucram putin sirul citit, si sa il afisam la stdout.
--   Cautati pe Hoogle functia getLine, vedeti ca ea are tipul IO String, si documentatia ne spune ca citeste o linie de la stdin.
--   Vedeti ca acolo mai sunt si alte functii IO, si pe noi pare sa ne intereseze putStrLn, care are tipul String -> IO ().
--   Vedem ca putStrLn ia un String, si il afiseaza la stdout.
--   Deci avem:
--     - getLine :: IO String
--     - putStrLn :: String -> IO ()
--   Cum putem sa folosim functiile astea doua? Vrem sa facem mai intai getLine, apoi sa luam rezultatul, sa aplicam o functie
--   String -> String pe el, si apoi sa ii dam string-ul asta prelucrat lui putStrLn.
--   Pai, cu (>>=) o sa reusim sa facem ce ne-am propus.
--   Stim ca IO are instanta de Monad, deci (>>=) :: IO a -> (a -> IO b) -> IO ().
--   (>>=) o sa ruleze acel IO a, apoi o sa dea rezultatul de tip a lui (a -> IO b).
--   In cazul nostru:
--     (>>=) :: IO String -> (String -> IO ()) -> IO ()
--   readProcessAndPrint o sa faca ce ne dorim:
--     1. getLine citeste o linie de la stdin
--     2. (>>=) ii da rezultatul lui processAndPrint
--     3. processAndPrint ia un String, aplica prelStr pe el (vedeti putin mai jos definitia lui prelStr),
--     si apoi afiseaza rezulatul cu putStrLn
-- Incercati sa rulati readProcessAndPrint in interpretor. O sa astepte un input de la voi si apoi il va afisa cu litere mari.

readProcessAndPrint :: IO ()
readProcessAndPrint =
    getLine >>= processAndPrint
  where
    processAndPrint :: String -> IO ()
    processAndPrint str =
        putStrLn (prelStr str)

prelStr :: String -> String
prelStr strin = map toUpper strin

-- Acum, ganditi-va ce s-ar intampla daca am avea mai multe computatii IO pe care sa vrem sa le secventiem asa.
-- Am avea o gramada de functii ajutatoare (functii ca processAndPrint). Am putea sa le scriem inline ca functii anonime,
-- dar am vedea ca nu devine neaparat mai citibil.
-- Aici intervine posibilitatea de a scrie astfel de cod folosind do-notation. Do-notation e un syntactic sugar, care seamana
-- foarte mult cu modul de a scrie programe in limbajele imperative. In spate, totul se transforma in computatii secventiate
-- cu (>>=) sau cu (>>) (cautati (>>) pe Hoogle, o sa vedeti ca face exact ce face (>>=) dar ignorand rezultatul primului argument).
-- De fapt, monadele modeleaza programarea imperativa. Deci putem programa imperativ in Haskell :)
-- readProcessAndPrint, scris cu do-notation, arata asa:

readProcessAndPrint' :: IO ()
readProcessAndPrint' = do
    str <- getLine  -- cu <- "extragem" valoarea rezultata din getLine
    putStrLn (prelStr str)

-- Incercati sa intelegeti ce face ioString:
ioString = do
    strin <- getLine
    putStrLn $ "Intrare\n" ++ strin
    let  strout = prelStr strin  -- putem folosi let ca sa denumim rezultate intermediare
    putStrLn $ "Iesire\n" ++ strout

-- Amintiti-va tipurile lui (>>=) si (>>):
--   - (>>=) :: Monad m => m a -> (a -> m b) -> m b
--   - (>>)  :: Monad m => m a ->       m b  -> m b
-- In spatele do-notation din ioString sta cod care arata cam ca ioString'.
-- Incercati sa gasiti ce tip are fiecare expresie de aici, si sa intelegeti ce se intampla/cum se executa.
ioString' =
    getLine >>= (\strin -> putStrLn ("Intrare\n" ++ strin) >> let strout = prelStr strin in putStrLn ("Iesire\n" ++ strout))

-- Incercati voi sa intelegeti exemplele urmatoare si sa faceti exercitiile din laborator.

prelNo noin = sqrt noin

ioNumber = do
    noin <- readLn :: IO Double
    putStrLn $ "Intrare\n" ++ show noin
    let  noout = prelNo noin
    putStrLn "Iesire"
    print noout

inoutFile = do
    sin <- readFile "Input.txt"
    putStrLn $ "Intrare\n" ++ sin
    let sout = prelStr sin
    putStrLn $ "Iesire\n" ++ sout
    writeFile "Output.txt" sout

-- Ca sa intelegem bine Monad, un exercitiu bun ar fi sa incercam sa definim noi un IO al nostru.
-- Simplificat, putem modela IO ca fiind o computatie care primeste un input, si intoarce urmatoarele:
--   - un rezultat
--   - ce a mai ramas din input (ce a ramas neconsumat)
--   - ce pune in output
-- Sau putem spune ca citeste din input si afiseaza in output, producand rezultate.

type Input = String  -- un alt nume pentru String
type Output = String  -- un alt nume pentru String

newtype MyIO a = MyIO { runIO :: Input -> (a, Input, Output)}
-- Tineti minte, pentru ca folosim record-syntax compilatorul genereaza urmatoarele functii
--   - MyIO :: (Input -> (a, Input, Output)) -> MyIO a  (constructorul)
--   - runIO :: MyIO a -> Input -> (a, Input, Output)

-- Vrem o computatie care citeste un caracter si il returneaza.
-- Putem implementa in felul urmator:
--   - Pune primul caracter din input ca rezultat, inputul nou e restul input-ului vechi, output nu avem.
--   - Daca input-ul e gol, putem sa aruncam o eroare sau sa returnam un caracter special, cum vreti voi.
-- O observatie (ca sa nu va induca in eroare diferentele de sintaxa):
--   In laboratorul 12 am definit functii asemanatoare, pentru MyRandom.
--   Atunci, am scris inline functia de sub constructor, cu sintaxa de functii anonime.
--   Putem, foarte bine, sa ii dam un nume functiei, ca mai jos, si sa o scriem intr-o clauza where, sau cu un let binding.
--   E acelasi lucru.
myGetChar :: MyIO Char
myGetChar = MyIO f
  where
    f :: Input -> (Char, Input, Output)
    f input =
        case input of
            []       -> ('\0', [], []) -- sau error "Empty input"
            (c : cs) -> (c, cs, [])

-- Rulati testMyGetChar in interpretor.
-- Amintiti-va de la laboratorul 12 cum "rulam" efectiv functiile incapsulate, ca MyRandom si ca MyIO.
testMyGetChar :: Bool
testMyGetChar = runIO myGetChar "Ana" == ('A', "na", "")

-- Aici vrem doar sa afisam rezulatul.
-- Deci, nu facem nimic cu Input, vrem doar sa punem c in output.
-- Returnam ().
myPutChar :: Char -> MyIO ()
myPutChar c = MyIO f
  where
    f :: Input -> ((), Input, Output)
    f input = ((), input, [c])

testMyPutChar :: Bool
testMyPutChar = runIO (myPutChar 'C') "Ana" == ((), "Ana", "C")

-- Ca sa putem defini instanta de Monad pe MyIO, trebuie sa avem mai intai instantele de Functor si de Applicative.

-- Ca deobicei, Functor ne lasa sa transformam rezultatul computatiilor. Vedeti ca implementarea seamana mult cu ce
-- am vazut la MyRandom.
-- Urmariti tipurile.
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

-- La fel, si la Applicative, instanta seamana destul de mult cu cea de la MyRandom.
-- Pentru ca modelam computatii unde inputul e consumat si outputul e generat,
-- vrem ca atunci cand compunem computatii sa pastram comportamentul asta.
-- Deci, in implementarea lui (<*>) o sa intoarcem cel mai nou input, si toate output-urile concatenate.
-- Pentru pure, trebuie doar sa intoarcem ce primim ca argument, fara sa modificam input-ul, si, pentru ca
-- nu vrem sa afisam nimic, output-ul va fi gol.
-- Iarasi, urmariti tipurile.
-- pure :: Applicative m => a -> m a
-- (<*>) :: Applicative m => m (a -> b) -> m a -> m b
instance Applicative MyIO where
    -- pure :: a -> MyIO a
    pure a = MyIO f
      where
        -- f :: Input -> (a, Input, Output)
        f input = (a, input, [])
    -- (<*>) :: MyIO (a -> b) -> MyIO a -> MyIO b
    MyIO fAToB <*> MyIO fa = MyIO fb
      where
        -- fAToB :: Input -> (a -> b, Input, Output)
        -- fa :: Input -> (a, Input, Output)
        -- input :: Input
        -- fb :: Input -> (b, Input, Output)
        -- resAToB :: a -> b
        -- resA :: a
        fb input =
            let (resAToB, newInput, output) = fAToB input
                (resA, newNewInput, newOutput) = fa newInput
             in (resAToB resA, newNewInput, output ++ newOutput)

-- Rulati si cititi testele de mai jos.
testPureMyIO :: Bool
testPureMyIO = runIO (pure 'C') "Ana" == ('C', "Ana", "")

testApMyIO :: Bool
testApMyIO = runIO (pure (<) <*> myGetChar <*> myGetChar) "Ana" == (True, "a", "")

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

-- Recomandare ca sa fiti siguri ca ati inteles Functor, Applicative si Monad:
--   Implementati instantele de Functor, Applicative si Monad pentru:
--     Identity, Maybe, Either, [], Reader, Writer, State
-- Vedeti in curs/cautati pe Hoogle sa vedeti cum arata tipurile astea.
--
-- Alte materiale:
--   1. Study plan https://github.com/soupi/haskell-study-plan
--   2. Haskell Book https://haskellbook.com/
--
-- Daca v-a placut Haskell/programarea functionala, si ati trecut deja prin toata materia din Haskell Book:
--     1. Thinking With Types https://leanpub.com/thinking-with-types
--     2. Some exercises https://github.com/i-am-tom/haskell-exercises
-- Functional Programming Community:
--     - r/haskell https://www.reddit.com/r/haskell/
--     - Functional Programming Slack https://fpchat-invite.herokuapp.com/
--     - Local community: Bucharest FP https://www.meetup.com/bucharestfp/ (currently inactive :( )
