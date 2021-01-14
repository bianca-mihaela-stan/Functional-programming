module Lib
    where
import Test.QuickCheck
import Data.Char


someFunc :: IO ()
someFunc = putStrLn "someFunc"


double :: Int -> Int
double x = x*2

triple :: Int -> Int
triple x = x*3

penta :: Int -> Int
penta x = x*5

test :: Int -> Bool
test x = (double x + triple x) == (penta x)

myLookUp :: Int -> [(Int, String)] -> Maybe String
myLookUp n l = process (filter (\(a, _) -> a==n) l )
    where
        process :: [(Int, String)] -> Maybe String
        process [] = Nothing
        process (x:xs) = Just (snd x)


testLookUp :: Int -> [(Int, String)] -> Bool
testLookUp n l = myLookUp n l == lookup n l




--asta se ruleaza dupa ce importam Test.QuickCheck
--cu generate arbitrary :: IO ElemIS


-- implementarea ar trebui sa fie conform specificatiei din prop_rotate,
-- incearca sa rulezi 'quickCheck prop_rotate' ca sa vezi pe ce cazuri
-- proprietatea nu tine;
-- ca sa folosesti quickCheck, poti sa creezi un proiect folosind Stack,
-- iar in package.yaml adaugi '- QuickCheck' sub '- base';
-- poti sa rulezi interpretorul din proiect cu 'stack ghci'


--2
--verifica ca o lista rotita cu k si rotita din nou cu lungime - k este identica cu cea initiala







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

cond1 :: Int -> String -> Property 
cond1 n list = n>0 && n<length list ==> prop_rotate n list

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
decipherListOfMaybeRec _ [] = ""
decipherListOfMaybeRec n str@(x:xs)
  | n<0 = error "n nu poate fi negativ!"
  | x == Nothing = decipherListOfMaybeRec n xs
  | otherwise = removeMaybe x : decipherListOfMaybeRec n xs

a::[Maybe Char] -> [Maybe Char]
a (x:xs) = xs

decipherListOfMaybeComp :: Int -> [Maybe Char] -> String
decipherListOfMaybeComp _ [] = ""
decipherListOfMaybeComp n str@(x:xs) = map removeMaybe (filter (\x -> not(x==Nothing)) str)

decipherStrRec :: Int -> String -> String
decipherStrRec n str@(y:ys) = decipherListOfMaybeRec n (map (decipher2 n) str)

decipherStrComp :: Int -> String -> String
decipherStrComp n str@(y:ys) = decipherListOfMaybeComp n (map (decipher2 n) str)

propTestDecipher :: Int -> String -> Property
propTestDecipher n str = n>0 && (not . null) str ==> decipherStrRec n str == decipherStrComp n str


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

--generator care foloseste generatorul de Int
testLookUpCond :: Int -> [(Int, String)] -> Property
testLookUpCond n list = n >0 && n `div` 5 == 0 ==> testLookUp n list

--generator custom

testlookUpCond' list= forAll myGen (\n -> testLookUp n list)
    where
        myGen :: Gen Int
        myGen = choose(1, 4)

data ElemIS = I Int | S String
    deriving (Show, Eq)
        
--asta se apeleaza cu generate arbitrary :: IO ElemIS
-- instance Arbitrary ElemIS where
--     arbitrary = do
--         intVal <- arbitrary :: Gen Int
--         strVal <- arbitrary :: Gen String
--         elements [(I intVal), (S strVal)]



--implementare care mai intai alege tipul si apoi genereaza
instance Arbitrary ElemIS where
    arbitrary = 
      let iGen = fmap I (arbitrary :: Gen Int)
          sGen = fmap S (arbitrary :: Gen String)
        in oneof [iGen, sGen]

--o 

--8 a)

myLookUp'Rec :: Int -> [(Int, String)] -> Maybe String
myLookUp'Rec  _ [] = Nothing
myLookUp'Rec n list@(x:xs) 
  | fst x == n = capitalize (snd x)
  | otherwise = myLookUp'Rec n xs
  where 
    capitalize :: String -> Maybe String
    capitalize [] = Just ""
    capitalize (y:ys) = Just (toUpper y : ys)

myLookUp'Cond :: Int -> [(Int, String)] -> Maybe String
myLookUp'Cond _ [] = Nothing
myLookUp'Cond n list@(x:xs)  = if (length m ==0) then Nothing else capitalize (snd (m!!0))
  where 
    m= filter (\x -> fst x == n) list
    capitalize :: String -> Maybe String
    capitalize [] = Just ""
    capitalize (y:ys) = Just (toUpper y : ys)


test_myLookUp' ::  Int -> [(Int, String)] -> Bool
test_myLookUp' n list = myLookUp'Cond n list == myLookUp'Rec n list

test_myLookUp'_myLookUp ::  Int -> [(Int, String)] -> Property
test_myLookUp'_myLookUp n list= and (map (\x -> snd x == "" ||  toUpper (head (snd x)) : tail (snd x) == snd x) list) == True ==> myLookUp n list ==myLookUp'Cond n list

--9
myLookUpElemRec :: Int -> [(Int, ElemIS)] -> Maybe ElemIS
myLookUpElemRec _ [] = Nothing
myLookUpElemRec c list@(x:xs) 
  | fst x == c = Just (snd x)
  | otherwise = myLookUpElemRec c xs

myLookUpElemComp :: Int -> [(Int, ElemIS)] -> Maybe ElemIS
myLookUpElemComp _ [] = Nothing
myLookUpElemComp c list@(x:xs) = if (length m ==0) then Nothing else Just (snd(m!!0))
  where m= filter (\(a,b) -> a==c) list

test_myLookUpElem :: Int -> [(Int, ElemIS)] -> Bool
test_myLookUpElem c list = myLookUpElemComp c list == myLookUpElemRec c list

---------------LABORATOR 5-----------------------

sumaPatrateImpare :: [Integer] -> Integer
sumaPatrateImpare list = sum (map (^2) (filter (\x -> x `mod` 2 == 1) list))


sumaPatrateImpare2 :: [Integer] -> Integer
sumaPatrateImpare2 list = foldr op 0 list
  where 
    op :: Integer -> Integer -> Integer
    op a suma 
      | odd a = a*a + suma
      | otherwise = suma



semnComp :: [Integer] -> String
semnComp list = foldr sm "" (filter (\x -> x>=(-9) && x<=9) list)
  where
    sm :: Integer -> String -> String
    sm x str 
      | x>0 = '+':str
      | x<0 = '-':str
      | x==0 = '0':str

semnRec :: [Integer] -> String
semnRec [] = ""
semnRec list@(x:xs)
  | x >= (-9) && x<0 = '-':semnRec xs
  | x==0 = '0':semnRec xs
  | x <= 9 && x>0 = '+':semnRec xs
  | otherwise = semnRec xs

test_semn :: [Integer] -> Bool
test_semn list = semnComp list == semnRec list

--1
corectComp :: [[a]] -> Bool
corectComp [] = True
corectComp list = and (map (\x -> length x ==m) list)
  where
    m=length (list!!0)

corectRec :: [[a]] -> Bool
corectRec [] = True
corectRec [x] = True
corectRec list@(x:(y:ys))
  | length x==length y = corectRec (y:ys)
  | otherwise = False

test_corect :: [[a]] -> Bool
test_corect list = corectComp list == corectRec list

--2
el :: [[a]] -> Int -> Int -> a
el matrix x y 
  | x >= length matrix = error $"x trebuie sa fie <=" ++ [intToDigit(length matrix -1)]
  | y >= lungime_max_linie = error  $"y trebuie sa fie <=" ++ [intToDigit(lungime_max_linie -1)] 
  | otherwise =(matrix!!x)!!y
  where
    lungime_max_linie = foldr max 0 (map (\x -> length x) matrix)


--3
merge :: [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys) = x: y: merge xs ys

place_in :: ([(a, Integer)], Integer) -> [(a, Integer, Integer)]
place_in (x,y) = [(a,y,b)|  (a,b) <- x]

transforma :: [[a]] -> [(a, Integer,Integer)]
transforma [] = []
transforma matrix@(y:ys) = foldr merge [] (map place_in (zip prima [0..]))
  where 
    prima = [zip x [0..] | x<-matrix]


transf_helper2 :: Int -> (Int, Int) -> (Int, Int, Int)
transf_helper2 a (b, c) = (c, a, b)

transf_helper1 :: ([(Int, Int)], Int) -> [(Int, Int, Int)]
transf_helper1 (a,b) =  map (transf_helper2 b) a

transforma2 :: [[Int]] -> [(Int, Int, Int)]
transforma2 m = let t= zip(map (zip [0..]) m) [0..] in foldr (++) [] (map transf_helper1 t)

functie1Comp :: [Int] -> [Int]
functie1Comp [] = error "Lista nu poate fi vida!"
functie1Comp list@(y:ys) = [fst x | x<-m, fst x == snd x]
  where
    m=(zip list ys)


functie1Rec :: [Int] -> [Int]
functie1Rec [] = error "Lista nu poate fi vida!"
functie1Rec [x] = []
functie1Rec list@(x:y:ys) 
  | x==y = y:functie1Rec (y:ys)
  | otherwise = functie1Rec (y:ys)

test_functie1 :: [Int] -> Property
test_functie1 list = (not.null) list ==> functie1Comp list == functie1Rec list


--rularea pe 3 tresete: se scrie in terminal :  quickCheckWith stdArgs { maxSuccess = 3 } test_functie1

functie2 :: Char -> Bool
functie2 c
  | c>='A' && c<='M' || c>='a' && c<='m' = True
  | isAlpha c == False = error "Caracterul trebuie sa fie litera!"
  | otherwise = False

functie4 :: Char -> Bool
functie4 x
  | isAlpha x = toLower x `elem` ['a'..'m']
  | otherwise = error "Caracterul trebuie sa fie litera!"

test_functie2_functie4 :: Char -> Property
test_functie2_functie4 c = isAlpha c ==> functie2 c == functie4 c

functie3Comp :: String -> Bool
functie3Comp str = sum(map (\x-> if functie2 x ==True then 1 else 0) m) > length m `div` 2
  where
    m= filter (\x -> isAlpha x) str

fhelper :: String -> Int
fhelper [] = 0
fhelper str@(x:xs) 
  | isAlpha x && functie2 x ==True = fhelper xs +1
  | isAlpha x && functie2 x ==False = fhelper xs-1
  | otherwise = fhelper xs

functie3Rec :: String -> Bool
functie3Rec str= fhelper str > 0

test_functie3 :: String -> Bool
test_functie3 str = functie3Rec str == functie3Comp str

data CharClass
  = Alphabetic Bool | NotAlphabetic
  deriving (Eq)

fSafe :: Char -> CharClass
fSafe c 
  | isAlpha c = Alphabetic (functie2 c)
  | otherwise = NotAlphabetic

g :: String -> Bool
g str = 
  let nrFirstHalf = length [ c | c<- str, fSafe c == Alphabetic True]
      nrSecondHalf = length [ c | c<-str, fSafe c == Alphabetic False]
    in nrFirstHalf > nrSecondHalf

h::String -> Bool
h str = 
  let nrFirstHalf = findNrWithCond isFirstHalf str
      nrSecondHalf = findNrWithCond isSecondHalf str
    in nrFirstHalf > nrSecondHalf
  where
    findNrWithCond :: (Char -> Bool) -> String -> Int
    findNrWithCond pred [] = 0
    findNrWithCond pred (c:cs)
      | pred c = 1+findNrWithCond pred cs
      | otherwise = findNrWithCond pred cs

    isFirstHalf c = fSafe c == Alphabetic True
    isSecondHalf c = fSafe c == Alphabetic False
  