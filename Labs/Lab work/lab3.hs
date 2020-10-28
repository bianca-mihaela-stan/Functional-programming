import   Data.List

-- L3.1 Încercati sa gasiti valoarea expresiilor de mai jos si
-- verificati raspunsul gasit de voi în interpretor:
{-
[x^2 | x <- [1 .. 10], x `rem` 3 == 2]
[(x, y) | x <- [1 .. 5], y <- [x .. (x+2)]]
[(x, y) | x <- [1 .. 3], let k = x^2, y <- [1 .. k]]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A' .. 'Z']]
[[x .. y] | x <- [1 .. 5], y <- [1 .. 5], x < y ]

-}

main= print(ordonataNat1 [1,2,3,4]);

factori :: Int -> [Int]
factori n = [x| x<-[1..n], n `rem` x ==0 ]

prim :: Int -> Bool
prim x 
  | length(factori x)==2 = True
  | otherwise =False

numerePrime :: Int -> [Int]
numerePrime n = [x | x <-[2..n], prim x ]

-- L3.2 Testati si sesizati diferenta:
-- [(x,y) | x <- [1..5], y <- [1..3]]
-- zip [1..5] [1..3]


--implementarea 1
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 (a:as) (b:bs) (c:cs) = (a, b, c) : myzip3 as bs cs
myzip3 _ _ _ =[]

--implementarea 2
unpack :: ((Int, Int), Int) -> (Int, Int, Int)
unpack ((a, b), c) =(a, b, c)


--myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
--myzip3 a b c = map unpack (zip (zip a b) c)

--------------------------------------------------------
----------FUNCTII DE NIVEL INALT -----------------------
--------------------------------------------------------
aplica2 :: (a -> a) -> a -> a
--aplica2 f x = f (f x)
--aplica2 f = f.f
--aplica2 f = \x -> f (f x)
aplica2  = \f x -> f (f x)

-- L3.3
{-
map(∗3 ) [ 1 , 3 , 4 ] => [3, 9, 12]
map($ 3 ) [ ( 4 +) , (10∗) , ( ^ 2 ) ,sqrt] => [ 7 . 0 , 3 0 . 0 , 9 . 0 , 1 . 7 3 2 0 5 0 8 0 7 5 6 8 8 7 7 2 ]
map (\ x -> 2 * x) [1 .. 10] -> 2, 4, 6..20
\ e lambda function
map (1 `elem` ) [[2, 3], [1, 2]] [False, True]
elem verifica daca exista elementul 1 in fiecare dintre liste
map ( `elem` [2, 3] ) [1, 3, 4, 5] [False,True,False,False]
mai sus aveam (1 `elem`) ceva, acum avem (`elem` [2,3]) ceva ceea ce inseamna ca avem de-a face cu sectiuni => va lua fiecare element din a doua lista si va verifica daca face parte din prima lista

-}

-- firstEl [ ('a', 3), ('b', 2), ('c', 1)]
first :: (a,b) -> a
first (a,b) =a

firstEl :: [(a, b)] ->[a]
--implementarea 1
--firstEl l = map(\x -> first x) l

--implementarea 2
firstEl l = map fst l



-- sumList [[1, 3],[2, 4, 5], [], [1, 3, 5, 6]]
sumList :: [[Integer]] -> [Integer]
sumList l = map sum l

-- prel2 [2,4,5,6]
process :: Integer -> Integer
process x 
  | x `rem` 2 ==0 =x `div` 2
  | otherwise = x * 2

prel2 :: [Integer] -> [Integer]
prel2 l = map process l

--L3.4

functie1:: Char -> [String] -> [String]
functie1 a b = filter (a `elem`) b

functie2 :: [Int] -> [Int]
functie2 x = map (\x -> x^2) (filter odd x)

functie3 :: [Int] -> [Int]
functie3 x = map (\(a, b) -> a^2) (filter (odd.snd) (zip x [1..length(x)]))

vocala :: Char -> Bool
--un string este un sir de vocale, din care aleg doar pe cele care sunt vocale
vocala x = x `elem` ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']


numaiVocale :: [String] -> [String]
numaiVocale b = map (filter(vocala)) b


--definiti recursiv functiile mymap si myfilter cu aceeasi functionalitate ca si functiile predefinite

--mymap f n = [f x | x<-n] 
mymap :: (a -> a) -> [a] -> [a]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs


myfilterRec :: (a -> Bool) -> [a] -> [a]
myfilterRec f [] = []
myfilterRec f (x:xs)
  | f x = x: (myfilterRec f xs)
  | otherwise = (myfilterRec f xs)

myfilterDesc :: (a -> Bool) -> [a] -> [a]
myfilterDesc f a= [x | x<-a, f x]

--exercitii suplimentare
--1
g:: (Int , Int) -> Bool
g (a,b) = a<b

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = and (map g  (zip (x:xs) xs))


--2
ordonataNat1 :: [Int] ->Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (a:xs)
  | a<b = True && ordonataNat1 xs
  | otherwise = False
  where
    b= xs!!0

--3
ordonata :: [a] -> (a->a->Bool) ->Bool
ordonata (x:xs) f = and(map f (zip (x:xs) xs))


    