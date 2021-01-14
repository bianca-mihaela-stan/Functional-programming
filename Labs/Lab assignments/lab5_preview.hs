import Data.Char
import Test.QuickCheck

main=QuickCheck prop_cd b c


f::Char -> Bool
f a
    | a>='A' && a<='M' = True
    | a>='a' && a<='m' = True
    | a>='N' && a<='Z' = False
    | a>='n' && a<='z' = False
    | otherwise = error "caracterul nu este din alfabet"

e :: String -> [Int]
e x = [ if (f s)==True then 1 else -1 | s<-x, isAlpha s]

g :: String -> Bool
g s = sum (e s) >0

d:: String -> [Int]
d [] =[]
d (x:xs) 
  | isAlpha x && f x==True = 1:t
  | isAlpha x && f x==False = -1:t
  | otherwise =t
  where 
    t=d xs


h :: String -> Bool
h s = sum (d s) >0



c::[Int]->[Int]
c (x:xs) = map (\ (a,b) -> a)  [(v, u)| (v, u)<- zip (x:xs) xs, v==u]


b::[Int]->[Int]
b [] = []
b [a] =[]
b (x:(y:xs))
  | y==x = x:t
  | otherwise = t
  where
    t=b (y:xs)


prop_cd :: ([Int]->[Int]) -> ([Int]->[Int]) -> Bool
prop_cd b c = b == c