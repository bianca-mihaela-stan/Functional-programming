import Data.Char

-----------------LABORATOR 6----------------------------

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

--se verifica proprietatea pe 100 de teste
-- cond1 :: Int -> String -> Property 
-- cond1 n list = n>0 && n<length list ==> prop_rotate n list

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
lookUp :: Char -> [(Char, Char)] -> Maybe Char
lookUp c l@(x:xs) 
  | l==[] = Nothing
  | c == fst x = Just (snd x)
  | xs==[] = Just c
  | otherwise = lookUp c xs

--5
encipher :: Int -> Char -> Char
encipher n c 
  | isAlpha c = chr ( (ord c + n - 65) `mod` 25 + 64 )
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
-- puteai folosi 'toUpper' pe toate caracterele pe care le pastrezi in sir,
-- pentru ca, de ex, "toUpper '4'" va fi tot '4';
-- dar e bine si cum ai facut tu :)

normalizeComp :: String -> String
normalizeComp x = map transformare (filter alpha_digit x)
  where
    alpha_digit :: Char -> Bool
    alpha_digit x = isAlpha x || isDigit x
    transformare :: Char -> Char
    transformare x = if isAlpha x then toUpper x else x
    -- aceeasi observatie ca mai sus

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
decipher :: Int -> Char -> Maybe Char
decipher n c 
  | c=='A' = Just (chr( ord 'Z' - (n-1)))
  | c=='B' = Just (chr( ord 'Z' - (n)))
  | c=='C' = Just (chr( ord 'Z' - (n+1)))
  | c=='D' = Just (chr( ord 'Z' - (n+2)))
  | c=='E' = Just (chr( ord 'Z' - (n+3)))
  | isAlpha c && toUpper c ==c = Just (chr (ord c - n))
  | isDigit c = Just c
  | c==' ' = Just c
  | otherwise = Nothing
--cred ca in cazul otherwise poti intoarce caracterul dat ca argument

decipher2 :: Int -> Char -> Maybe Char
decipher2 n c 
  | isAlpha c && toUpper c ==c = Just (chr ( (ord c - n - 65 +2) `mod` 25 + 64 ))
  | isDigit c || c==' ' = Just c
  | otherwise = Nothing

removeMaybe :: Maybe Char -> Char
removeMaybe Nothing = error "Nu am caracter gol!"
removeMaybe (Just x) = x

decipherListOfMaybeRec :: Int -> [Maybe Char] -> String
decipherListOfMaybeRec n str@(x:xs)
  | n<0 = error "n nu poate fi negativ!"
  | str==[] = ""
  | x == Nothing = decipherListOfMaybeRec n xs
  | otherwise = removeMaybe x : decipherListOfMaybeRec n xs

a::[Maybe Char] -> [Maybe Char]
a (x:xs) = xs

decipherListOfMaybeComp :: Int -> [Maybe Char] -> String
decipherListOfMaybeComp n str@(x:xs) = map removeMaybe (filter (\x -> not(x==Nothing)) str)

decipherStrRec :: Int -> String -> String
decipherStrRec n str@(y:ys) = decipherListOfMaybeRec n (map (decipher2 n) str)

decipherStrComp :: Int -> String -> String
decipherStrComp n str@(y:ys) = decipherListOfMaybeComp n (map (decipher2 n) str)

-- propTestDecipher :: Int -> String -> Property
-- propTestDecipher n str = n>0 && (not . null) str ==> decipherStrRec n str == decipherStrComp n str


-- E interesanta abordarea asta care foloseste reprezentarea numerica
-- a caracterelor, am vazut ca ati avut mai multi ideea asta dar tu ai implementat
-- toate exercitiile asa. Cred ca scopul exercitiilor era sa folositi 'rotate'
-- si restul functiilor ajuatoare ca 'lookUp' ca sa implementati cifrul.
-- E foarte bine ca ai alta implementare, dar daca ai timp, cred ca e good practice
-- sa incerci sa implementezi si cum sugera laboratorul. :)


--a)
data Fruct
  = Mar String Bool
  | Portocala String Int
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s _) = elem s ["Moro", "Sanguinello","Tarocco"]

--sau
-- ePortocalaDeSicilia2 f = 
--    case f of
--        Porotcala soi _ -> soi `elem` ["Moro", "Sanguinello","Taroco"]
--                        -> False

 --sau
ePortocalaDeSicilia (Portocala "Moro" _) = True
ePortocalaDeSicilia (Portocala   "Taroco" _) = True
ePortocalaDeSicilia (Portocala   "Sanguinello" _) = True
ePortocalaDeSicilia  _ = False

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
nrFeliiSicilia l = sum (map getFelie(filter ePortocalaDeSicilia l))
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
verifica m n = and (map (eq n) (map sum (map getListe (getLinii m))))
  where
    eq :: Int -> Int -> Bool
    eq x n = x==n
-- foldr (&&) True e echivalent cu and
-- sectiunea (== n) e echivalenta cu eq
-- dar e bine si asa


helper1 :: [[Int]] -> Int -> Bool
helper1 [] _ = True
helper1 (x:xs) n 
  | sum x ==n = helper1 xs n
  | otherwise = False

verificaRec :: Matrice -> Int -> Bool
verificaRec m n = helper1 (map getListe (getLinii m)) n

--b
--Nu prea stiu cum ar trebui sa fac asta

-- cred ca ti-am raspuns la laborator, daca tot e ceva neclar,
-- nu ezita sa imi scrii si o sa incerc sa revin cu mai multe explicatii

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


------------------LABORATOR 7------------------------

data ElemIS = I Int | S String
  deriving (Show, Eq)

myLookUpElemRec :: Int -> [(Int, ElemIS)] -> Maybe ElemIS
myLookUpElemRec c list@(x:xs) 
  | list == [] = Nothing
  | fst x == c = Just (snd x)
  | otherwise = myLookUpElemRec c xs

myLookUpElemComp :: Int -> [(Int, ElemIS)] -> Maybe ElemIS
myLookUpElemComp c list@(x:xs) = if (m == []) then Nothing else Just (snd(m!!0))
  where m= filter (\(a,b) -> a==c) list

test_myLookUpElem :: Int -> [(Int, ElemIS)] -> Bool
test_myLookUpElem c list = myLookUpElemComp c list == myLookUpElemRec c list




--Haskell tutorial
database :: [(Integer, String)] 
database = [(1, "Julie"), (2, "Chris"), (3, "Aurora"), (4, "Mathew")]

greetUser :: Integer -> Maybe String --lookup returns a maybe string
greetUser record =
    --putem sa folosim fmap pentru chestia astaaaa
    fmap ("Hello " ++ ) (lookup record database)


mapToMaybe :: (a -> b) -> Maybe a -> Maybe b
mapToMaybe func Nothing = Nothing
mapToMaybe func (Just string) = Just (func string)

-- generalMap :: (a->b) -> f a -> f b
-- generalMap 

-- mapToEither :: (a-> b) -> Either left a -> Either left a
-- mapToEither func (Left l) = Left l
-- mapToEither func (Right a) = Right (func a)

