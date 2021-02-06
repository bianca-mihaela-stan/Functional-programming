fib :: Int -> Integer
fib = f
    where 
        f 0 = 0
        f 1 = 1
        f n = (genf !! (n-2)) + (genf !! (n-1))

        genf = map f [0..] --o sa mapeze f pe [0..] care da 1 la 1 => o sa am 0 +1+1+1+1+1 pt fib 5


zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' f = go
    where go [] _ = []
          go _ [] = []
          go (x:xs) (y:ys) = f x y : go xs ys 

fibs :: [Integer]
fibs = 0 : 1 : zipWith' (+) fibs (tail fibs)

--rescrierea functiei elem
--cu descrieru de liste
elem1 x ys = or [x==y |  y<- ys]
--cu recursivitate
elem2 x [] = False
elem2 x (y:ys) = x == y | elem2 y ys
--cu functii de nivel inalt
elem x ys = foldr (||) False (map (x ==) ys)


--clase de tipuri
class Eq a where
    (==) ::a -> a -> Bool
    (/=) :: a -> a -> Bool
    --minimum definition of ==
    x/=y = not(x == y)

--instanta
instance Eq () where
    () == () = True

instance Eq Bool where
    False == False = True
    False == True = False
    True == False = False
    True == True = True

instance Eq Int where
    (==) = eqInt --builtin

instance Eq Char where
    x == y = ord x == ord y

instance Eq a => Eq [a] where
    [] == [] = True
    [] == y:ys = False
    y:ys == [] = False
    x:xs == y:ys = (x==y) && (xs==ys)

--trebuie sa mentionam ca elem face parte din clasa Eq:   elem :: Eq a => a -> [a] -> Bool


--extinderea unei clase
class (Eq a) => Ord a where
    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    --minimum definition of <=
    x<y = x<=y && x/=ys
    x>y = y<x
    x>=y = y<=x


instance Ord Bool where
    False <= False = True
    False <= True = True
    True <= False = False
    True <= True = True

instance (Ord a, Ord b) => Ord(a,b) where
    (x,y) <= (x', y') = x<x' || (x==x' && y<=y')

class Show a where
    show :: a -> String

instance Show Bool where
    show Fa;se


--tipuri de date algebrice

--tip de

--season e constructor de ttip
--spring summer etc sunt constructori de date
data Season = Spring | Summer | Autumn | Winter

--clasa de tipuri pentru datele care pot fi adisate
class Visible a where
    toString :: a -> String

instance Visible Char where
    toString c = [c]

class Show a where
    show :: a -> String

instance Show Bool where
    show False = "False"
    show True = "True"

instance (Show a, Show b) => Show (a,b) where
    show (x,y) = "(" ++ show x ++ ","++ show y ++ ")"


instance Show a => Show [a] where
    show [] = "[]"
    show (x:xs) = "["++showSep x xs ++ "]"
        where
            showSep x [] = show x
            showSep x (y:ys) = show x ++ " ," ++ showSep y ys


class (Eq a, Show a) => Num a where
    (+), (-), (*) :: a -> a -> a
    negate :: a-> a
    fromInteger :: Integer -> a
    --minimum definition of (+), (-), (*), fromInteger
    negate x = fromInteger 0-x

class (Num a) => Fractional a where
    (/) :: a -> a-> a
    recip :: a-> a
    fromRational :: Rational -> a
    --minimum definition of /, fromRational
    recip x = 1/x

class (Num a, Ord a) => Real a where
    toRational :: a -> Rational

class (Real a, Enum a) => Interal a where

-- reverse-words.hs
main = interact $ unlines . zipWith (++)
  ["Case #" ++ show t ++ ": " | t <- [1..]] . map solve . tail . lines

solve = unwords . reverse . words