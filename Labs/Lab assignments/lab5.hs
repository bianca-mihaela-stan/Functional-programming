main=print(transforma [[1, 2, 3], [4, 5, 6], [7, 8, 9]])

corect :: [[a]]-> Bool
corect [] = True
corect [a] = True
corect (a:(b:bs)) 
  | length a == length b = True && corect(b:bs)
  | otherwise = False


el :: [[a]] -> Int -> Int -> a
el a x y= if corect a then (if x<length a  && y<length (a!!0) then (a!!x)!!y else error "x si y nu sunt in intervalul [0..n-1][0..m-1]") else error "Matricea nu este corecta"

--unpack :: ([Int], Int) -> [(Int, Int)]
--unpack x = let b=snd x in [ (a,b) | a<-fst x]

transf_helper2 :: Int -> (Int, Int) -> (Int, Int, Int)
transf_helper2 a (b, c) = (c, a, b)

transf_helper1 :: ([(Int, Int)], Int) -> [(Int, Int, Int)]
transf_helper1 (a,b) =  map (transf_helper2 b) a

transforma :: [[Int]] -> [(Int, Int, Int)]
transforma m = let t= zip(map (zip [0..]) m) [0..] in foldr (++) [] (map transf_helper1 t)
