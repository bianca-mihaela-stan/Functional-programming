main=print(semn [5, 10, -5, 0])


rnChar :: Char -> String -> String
rnChar a b = filter (a/=) b
--sau asa: [x | x<-b, x/=a]

--b
rnCharsRec :: String -> String -> String
rnCharsRec b [] =[]
rnCharsRec [] [] = []
rnCharsRec [] b = b
rnCharsRec a (b:bs) 
  | elem b a = rnCharsRec a bs
  | otherwise = b:rnCharsRec a bs
--altfel : rmCharsRec (x:xs) str =rmCharsRec x (rmCharsRec xs str)


--c
rmCh ::String -> Char ->[Char]
rmCh s c
  | elem c s =[]
  | otherwise =[c]

rnCharsFold :: String -> String -> String
rnCharsFold a b = foldr (++) [] (map(rmCh a) b) 

rmCharsFold charsToRemove str = foldr f unit charsToRemove
  where 
    unit =str
    f = rnChar

--pana nu afisam nu se evalueaza



semn :: [Int] -> String
semn (a:as)
  | a<=9 && a>0 = "+"++(semn as)
  | a>=-9 && a<0 ="-"++(semn as)
  | a==0 = "0"++(semn as)
  | otherwise = semn as

  --op:; Integer -> String -> String
  -- unit :: String

semnFold :: [Integer] -> String
semnFold = foldr op unit
  where
    unit = ""
    x `op` rez
      | a<=9 && a>0 = '+'++(semn as)
      | a>=-9 && a<0 ='-'++(semn as)
      | a==0 = "0"++(semn as)
      | otherwise = semn as
