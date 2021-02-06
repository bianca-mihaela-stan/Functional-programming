-------------------------------SUBIECTUL 1---------------------------------
combina :: [a] -> [a] -> [a]
combina x y =foldr ((++) . (\(a,b) -> [a,b])) [] (zip x y)

--teste facute
-- combina [1,3,5,7, 9] [2,3,6]
-- [1,2,3,3,5,6]
-- combina "ana" "mere" 
-- "amnear"
-- combina [9,7..] [8,6..0]
-- [9,8,7,6,5,4,3,2,1,0]

--------------------------------SUBIECTIUL 2-------------------------------

data Exp = Var String | Num Int | Exp :+: Exp | Exp :/: Exp

type Env =[(String, Int)]

-- data Val = VNum Int | Eroare
-- interp :: Exp -> Reader Env Val
-- interp (Var x) = do env <-ask
--                     case lookup x env of
--                         Nothing -> Eroare
--                         Just i -> return (VNum i)

-- interp (Num i) = return (VNum )