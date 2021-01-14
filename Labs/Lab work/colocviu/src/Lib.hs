module Lib
where
import Test.QuickCheck

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--am verificat pe ["aba"] -> True si ["aab"] -> Flase ["abc", "abba"] -> True
sbr1 :: [String] -> Bool
sbr1 [] = False
sbr1 list@(x:xs)
    | x==reverse x = True
    | otherwise = sbr1 xs

--zip la descrieri de liste

sbr2 :: [String] -> Bool
sbr2 list = length [x| x<-list, x==reverse x] >0

sbr3 :: [String] -> Bool
sbr3 list= length (filter (\x -> x==reverse x) list) >0

sbr123 :: [String] -> Bool
sbr123 list = sbr1 list == sbr2 list && sbr2 list == sbr3 list 


sbr13 :: [String] -> Bool
sbr13 list = sbr1 list == sbr3 list 

sbr4 :: [Int] -> [Int]
sbr4 [] = []
sbr4 [a] =[a]
sbr4 [a,b] = if a==b && a==0 then [0] else [a,b]
sbr4 [a,b,c] 
    | a==b && b==c && c==0 = [0]
    | a==b && b==0 = [0,c]
    | b==c && c==0 =[a,0]
    | otherwise = [a,b,c]
sbr4 list@(x:y:z:zs)
    | x==y && y==0 = sbr4 (y:z:zs)
    | otherwise = x: sbr4 (y:z:zs)

sbr5 :: [Int] -> [Int]
sbr5 []=[]
sbr5 [a] = [a]
sbr5 (x:y:ys)
    | x==0 && y==1 = sbr5(y:ys)
    | otherwise = x:sbr5(y:ys)


--am testat pe [3,0,0,1,0,2,0,0] -> [3,1,2,0]
--[3,0,0,1,0,2,0,0,4,5,0] -> [3,1,2,0,4,5,0]
--[0,0,0] -> [0]
--[1,0,1] -> [1,1]
sbr6 :: [Int] -> [Int]
sbr6 list = reverse eliminare_1_inainte_de_0
    where
        eliminare_0 = sbr4 list
        eliminare_0_inainte_de_1 = sbr5 eliminare_0
        eliminare_1_inainte_de_0 = sbr5 (reverse eliminare_0_inainte_de_1)


sbr41 :: [Int] -> [Int]
sbr41  [] = []
sbr41 [a] = [a]
sbr41 list@(y:ys) = [fst x| x<-m, fst x== snd x && fst x /=0 || fst x==0 && snd x /=0 || fst x /=0 && snd x ==0]
    where 
        m= zip list ys

test_sbr4_sbr41 :: [Int] -> Bool
test_sbr4_sbr41 list = sbr4 list == sbr41 list