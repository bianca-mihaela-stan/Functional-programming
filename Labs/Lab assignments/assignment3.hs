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
ordonata :: [a] -> (a->a->Bool) -> Bool
ordonata (x:xs) f =and (map (\(a, b) -> f a b) (zip (x:xs) xs)) 

(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(*<*) (a,b) (c,d) = a==d && b==c

--4

compunereList :: (b->c) -> [(a->b)] -> [(a->c)]
compunereList f g = [f.h | h<-g]


aplicaList :: a -> [(a->b)] -> [b]
aplicaList x y = [z x | z<- y]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 a b c = map unpack (zip (zip a b) c)
