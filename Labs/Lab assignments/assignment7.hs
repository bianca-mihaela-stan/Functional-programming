import Data.Char

--1

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

rotate:: Int -> String -> String
rotate n l
  | n<=0 = error("n needs to be >0")
  | n>=length l = error("n needs to be < length of the string")
  | otherwise = t
  where
    t=(slice n (length l) l)++(slice 0 (n-1) l)



--2
--verifica ca o lista rotita cu k si rotita din nou cu lungime - k este identica cu cea initiala
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

--3
next_n :: Char ->Int -> Char
next_n c n
  | c=='V' = 'A'
  | c=='W' = 'B'
  | c=='X' = 'C'
  | c=='Y' = 'D'
  | c=='Z' = 'E'
  | otherwise = chr (ord c + n)

next_n2 :: Char ->Int -> Char
next_n2 c n= chr ( (ord c + n - 65) `mod` 25 + 64 )

makeKey :: Int -> [(Char, Char)]
makeKey n = [(chr x, next_n (chr x) n) | x<-[65..90]]

--4
lookUp :: Char -> [(Char, Char)] -> Char
lookUp _ [] = error("Nu se gaseste elementul.")
lookUp c (x:xs) 
  | c == fst x = snd x
  | xs==[] = c
  | otherwise = lookUp c xs

--5
encipher :: Int -> Char -> Char
encipher n c 
  | c=='V' = 'A'
  | c=='W' = 'B'
  | c=='X' = 'C'
  | c=='Y' = 'D'
  | c=='Z' = 'E'
  | isAlpha c = chr (ord c + n)
  | otherwise = c

encipher2 :: Char ->Int -> Char
encipher2 c n= chr ( (ord c + n - 65) `mod` 25 + 64 )

--6
normalize :: String -> String
normalize [] = []
normalize (x:xs)
  | isAlpha x = toUpper x:t
  | isDigit x = x:t
  | otherwise = t
  where
    t=normalize xs

normalizeComp :: String -> String
normalizeComp x = map transformare (filter alpha_digit x)
  where
    alpha_digit :: Char -> Bool
    alpha_digit x = isAlpha x || isDigit x
    transformare :: Char -> Char
    transformare x = if isAlpha x then toUpper x else x

--7
encipherStr :: Int -> String -> String
encipherStr n str = let nor = normalize str in [ encipher n x | x<-nor ]

--8
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey str = [(snd x, fst x) | x<-str]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs) = (snd x, fst x):reverseKeyRec xs

--9
decipher :: Int -> Char -> Char
decipher n c 
  | c=='A' = 'V'
  | c=='B' = 'W'
  | c=='C' = 'X'
  | c=='D' = 'Y'
  | c=='E' = 'Z'
  | isAlpha c && toUpper c ==c = chr (ord c - n)
  | isDigit c = c
  | c==' ' = c
  | otherwise = ' ' --nu am gasit o metoda sa am charvgol aici

decipher2 :: Int -> Char -> Char
decipher2 n c 
  | isAlpha c && toUpper c ==c = chr ( (ord c + n - 65) `mod` 25 + 64 )
  | isDigit c || c==' ' = c
  | otherwise = ' '

decipherStr :: Int -> String -> String
decipherStr n str = [decipher n x | x<- str]


--a)
data Fruct
  = Mar String Bool
  | Portocala String Int
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s _) = elem s ["Moro", "Sanguinello","Tarocco"]

--sau
--ePortocalaDeSicilia f = 
   -- case f of
       -- Porotcala soi _ -> soi `elem` ["Moro", "Sanguinello","Taroco"]
             --           -> False

 --sau
--  ePortocalaDeSicilia (Porocala "Moro" _) = True
--  ePortocalaDeSicilia (Porocala   "Taroco" _) = True
--   ePortocalaDeSicilia (Porocala   "Sanguinello" _) = True
--   ePortocalaDeSicilia  _ = False

--b
listaFructe =
    [ Mar "Ionatan" False
    , Portocala "Sanguinello" 10
    , Portocala "Valencia" 22
    , Mar "Golden Delicious" True
    , Portocala "Sanguinello" 15
    , Portocala "Moro" 12
    , Portocala "Tarocco" 3
    , Portocala "Moro" 12
    , Portocala "Valencia" 2
    , Mar "Golden Delicious" False
    , Mar "Golden" False
    , Mar "Golden" True
    ]
  
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum(map getFelie(filter ePortocalaDeSicilia l))
                   where
                       getFelie :: Fruct -> Int
                       getFelie (Portocala _ nr) = nr
                       getFelie (Mar _ _) = 0


--c
nrMereViermi :: [Fruct] -> Int
nrMereViermi l = length (filter eMarcuviermi l)
                 where
                     eMarcuviermi :: Fruct -> Bool
                     eMarcuviermi (Mar _ viermi) = (viermi == True)
                     eMarcuviermi _ = False


data Linie = L [Int]
  deriving Show
data Matrice = M [Linie]

--Matrice
--a

--main= print(verifica (M[L[2,20,3],L[4,21],L[2,3,6,8,6],L[8,5,3,9]])25)

getLinii :: Matrice -> [Linie]
getLinii (M a) = a

getListe :: Linie -> [Int]
getListe (L a) = a 

verifica :: Matrice -> Int -> Bool
verifica m n = foldr (&&) True (map (eq n) (map sum (map getListe (getLinii m))))
  where
    eq :: Int -> Int -> Bool
    eq x n = x==n


helper1 :: [[Int]] -> Int -> Bool
helper1 [] _ = True
helper1 (x:xs) n 
  | sum x ==n = helper1 xs n
  | otherwise = False

verificaRec :: Matrice -> Int -> Bool
verificaRec m n = helper1 (map getListe (getLinii m)) n

--b
--Nu prea stiu cum ar trebui sa fac asta


--c

doarPozN :: Matrice -> Int -> Bool
doarPozN m n = foldr (&&) True (map (eq n) (map sum_pos matrice))
  where 
    all_pos :: [Int] -> Bool
    all_pos [] = True
    all_pos (x:xs) = if x<=0 then False else all_pos xs
    sum_pos :: [Int] -> (Int, Bool)
    sum_pos l = (length l, all_pos l)
    eq ::  Int -> (Int, Bool) -> Bool
    eq n x= if fst x == n && snd x == False then False else True
    matrice = map getListe (getLinii m)--matrice e de tipul [[Int]]

helper2 :: [[Int]] -> Int -> Bool
helper2 [] _ = True
helper2 (x:xs) n 
  | length x == n && all_pos x ==True = helper2 xs n
  | length x /= n = True
  | otherwise = False
  where
    all_pos :: [Int] -> Bool
    all_pos [] = True
    all_pos (x:xs) = if x<=0 then False else all_pos xs

doarPozNRec :: Matrice -> Int -> Bool
doarPozNRec m n = helper2 (map getListe (getLinii m)) n