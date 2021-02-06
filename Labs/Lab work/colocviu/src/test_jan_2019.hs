import Data.List (lookup)
import Data.Char (chr, ord)

type Linie = Int 
type Coloana = Char 
type Pozitie = (Coloana, Linie)

type DeltaLinie = Int
type DeltaColoana = Int
type Mutare = (DeltaColoana, DeltaLinie)

-----------------------------------------EXERCITIUL 1--------------------------------------

verificare_col :: Coloana -> DeltaColoana -> Bool 
verificare_col col del_col = ord(col)+del_col >= ord('a') && ord(col)+del_col <=ord('h')

verificare_lin :: Linie -> DeltaLinie -> Bool 
verificare_lin lin del_lin = lin+del_lin >= 1 && lin+del_lin <=8

mutaDacaValid :: Pozitie -> Mutare -> Pozitie
mutaDacaValid (col, lin) (del_col, del_lin) 
    | verificare_col col del_col && verificare_lin lin del_lin = (chr(ord(col)+del_col), lin+del_lin)
    | otherwise = (col, lin)

--teste facute:
-- mutaDacaValid ('e', 5) (1, -2) == ('f', 3)
-- True
-- mutaDacaValid ('b', 5) (-2, 1) == ('b', 5) 
-- False
-- mutaDacaValid ('e', 2) (1, -2) == ('e', 2)
-- False

---------------------------------------EXERCITIUL 2--------------------------------------

mutariPosibile :: [Mutare]
mutariPosibile = [(-2, -1), (-2, 1), (2, -1), (2, 1), (-1, -2),(1, -2), (-1, 2), (1, 2)]

type IndexMutare = Int
type Joc = [IndexMutare]

type DesfasurareJoc = [Pozitie]

joaca :: Pozitie -> Joc -> DesfasurareJoc
-- joaca p j= [ mutaDacaValid p (mutariPosibile!!x) | x<-j, x>=0 && x<=7, mutaDacaValid p (mutariPosibile!!x) /= p]
joaca p []= [p]
-- joaca p (x:xs)= if x>=0 && x<=7 && (pozitie_finala /= p ) then p : joaca pozitie_finala xs else p:joaca p xs
--     where
--         pozitie_finala = mutaDacaValid p (mutariPosibile!!x)
joaca p (x:xs)= if x>=0 && x<=7 then 
                    if (pozitie_finala /= p ) 
                    then p : joaca pozitie_finala xs 
                    else joaca p xs
                else joaca p xs
    where
        pozitie_finala = mutaDacaValid p (mutariPosibile!!x)

--teste facute
-- joaca ('a', 8) [0,3,2,7]
-- [('a',8),('c',7)]
-- joaca ('e', 5) [0,3,9,2,7]
-- [('e',5),('c',4),('e',5),('g',4),('h',6)]
-- joaca ('e', 5) [0,3,2,7]
-- [('e',5),('c',4),('e',5),('g',4),('h',6)]

--------------------------------------EXERCITIUL 3---------------------------------------
data ArboreJoc = Nod Pozitie [ArboreJoc]
    deriving (Show, Eq)

parcurge:: Int -> ArboreJoc -> ArboreJoc
parcurge adancime (Nod p as)
    |adancime <= 0 = Nod p []
    | otherwise = Nod p (map (parcurge (adancime -1 )) as)


genereaza_pentru_mutarea_x :: Pozitie -> [Mutare] -> [Pozitie] ->[ArboreJoc]
genereaza_pentru_mutarea_x p [] _= []
genereaza_pentru_mutarea_x p (x: xs) pozitii_generate
    | not(elem pozitie_finala pozitii_generate) = Nod pozitie_finala (genereaza_pentru_mutarea_x pozitie_finala mutariPosibile (pozitie_finala:pozitii_generate)) : (genereaza_pentru_mutarea_x p xs (pozitii_generate))
    | otherwise = genereaza_pentru_mutarea_x p xs pozitii_generate
    where 
        pozitie_finala = (mutaDacaValid p x)


genereaza :: Pozitie -> ArboreJoc
genereaza p = Nod p (genereaza_pentru_mutarea_x p mutariPosibile [p])


--teste facute
-- parcurge 2 (genereaza ('a', 1))
-- Nod ('a',1) [Nod ('c',2) [Nod ('a',3) [],Nod ('e',1) [],Nod ('e',3) [],Nod ('b',4) [],Nod ('d',4) []],Nod ('b',3) [Nod ('d',2) [],Nod ('d',4) [],Nod ('c',1) [],Nod ('a',5) [],Nod ('c',5) []]]
-- parcurge 1 (genereaza ('a', 1))                                       
-- Nod ('a',1) [Nod ('a',1) [],Nod ('a',1) [],Nod ('a',1) [],Nod ('c',2) [],Nod ('a',1) [],Nod ('a',1) [],Nod ('a',1) [],Nod ('b',3) []]
-- parcurge 1 (genereaza ('e', 5))
-- Nod ('e',5) [Nod ('c',4) [],Nod ('c',6) [],Nod ('g',4) [],Nod ('g',6) [],Nod ('d',3) [],Nod ('f',3) [],Nod ('d',7) [],Nod ('f',7) []]

-------------------------------------------EXERCITIUL 4------------------------------------
newtype JocWriter a = Writer {runWriter :: (a, Joc)}

scrie :: IndexMutare -> JocWriter()
scrie i = Writer ((), [i])

instance Monad JocWriter where
    return a = Writer (a, [])
    ma >>= k = let (x, jocM) = runWriter ma
                   (y, jocK) = runWriter (k x)
                in Writer (y, jocM ++ jocK)

instance Functor JocWriter where
    fmap f ma = ma >>= return . f

instance Applicative JocWriter where
    pure = return
    mf <*> ma = mf >>= (<$> ma)


-------------------------------------EXERCITIUL SUPLIMENTAR---------------------------------
verifica_mutarea_x :: Pozitie ->Pozitie -> IndexMutare -> Int 
verifica_mutarea_x p1 p2 index_mutare 
    | mutaDacaValid p1 (mutariPosibile!!index_mutare) == p2 = index_mutare
    | otherwise = -1


gasesteMutare :: Pozitie -> Pozitie -> Maybe IndexMutare
gasesteMutare p1 p2 
    | (length filtrat) > 0 = Just (filtrat!!0)
    | otherwise = Nothing 
    where
        lista = map (verifica_mutarea_x p1 p2) [0..7]
        filtrat = filter (>=0) lista

--teste facute
-- gasesteMutare ('e', 5) ('f', 3) 
-- Just 5
-- gasesteMutare ('a', 8) ('c', 7)
-- Just 2
-- gasesteMutare ('e', 5) ('f', 6)
-- Nothing
