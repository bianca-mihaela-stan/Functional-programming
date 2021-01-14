module Lib
    ( someFunc
    ) where
import Test.QuickCheck

someFunc :: IO ()
someFunc = putStrLn "someFunc"

take' :: Int -> [a]->[a]
take' _ [] = []
take' 1 (x:_) = [x]
take' m (x:xs) = x: take' (m-1) xs

zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' f = go
    where go [] _ = []
          go _ [] = []
          go (x:xs) (y:ys) = f x y : go xs ys 
    
fibs :: [Integer]
fibs = 0 : 1 : zipWith' (+) fibs (tail fibs)


fib :: Int -> Integer
fib = f
    where 
        f 0 = 0
        f 1 = 1
        f n = (genf !! (n-2)) + (genf !! (n-1))

        genf = map f [0..]


myLookUp :: Int -> [(Int, String)] -> Maybe String
myLookUp n l = process (filter (\(a, _) -> a==n) l )
    where
        process :: [(Int, String)] -> Maybe String
        process [] = Nothing
        process (x:xs) = Just (snd x)


testLookUp :: Int -> [(Int, String)] -> Bool
testLookUp n l = myLookUp n l == lookup n l