import Data.Char

--L2.4

main = print (discountRec  [150, 300, 250, 200, 450, 100] )

functie_ajutatoare :: Int -> [Int] -> [Int]
functie_ajutatoare x y
  | null y = y
  | odd h = (x-length y) : t'
  | otherwise = t'
  where
    h= head y
    t= tail y
    t' = functie_ajutatoare x t


pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec p = functie_ajutatoare (length p) p

pozitiiImpareComp :: [Int] ->[Int]
pozitiiImpareComp l = [i | (i,x) <- [0..] `zip` l, odd x ]

--L2.5


multDigitsRec :: [Char] -> Int
multDigitsRec x 
  | null x = 1
  | isDigit h = t'* (digitToInt h)
  | otherwise = t'
  where
    h=head x
    t=tail x
    t'=multDigitsRec t

    


discountRec :: [Integer] -> [Float]
discountRec x 
  | null x = []
  | fromInteger h*0.75<200 = fromInteger h *0.75 : t'
  | otherwise = t'
  where
    h = head x
    t = tail x
    t' = discountRec t

discountComp :: [Integer] -> [Float]
discountComp x = [fromInteger h *0.75| h<-x, fromInteger h*0.75<200]