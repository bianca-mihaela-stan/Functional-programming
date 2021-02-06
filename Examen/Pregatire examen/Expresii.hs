module Expresii where
import Data.List (nub, subsequences)
import Data.Maybe (fromJust)

type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    | Prop :->: Prop
    | Prop :<->: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:
---------------------------------EXERCITIUL 1------------------------

--1.1 (P sau Q) si (P si Q)
p1::Prop
p1=(Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

--1.2 (P sau Q) si (not P si not Q)
p2 :: Prop
p2=(Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not(Var "Q"))

--1.3 (P si (Q sau R)) si ((not P sau not Q) si (not P sau not R))
p3 :: Prop
p3=(Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not(Var "Q")) :&:(Not (Var "P") :|: Not (Var "R")))


--2. Faceti tipul Prop instanta a clasei Show

showProp :: Prop -> String
showProp (Var p) = p 
showProp F = "F"
showProp T = "T"
showProp (Not p) = "(~" ++ showProp p ++ ")"
showProp (p :|: q) = "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q) = "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q) = "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q) = "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

instance Show Prop where
    show = showProp

test_ShowProp :: Bool
test_ShowProp = show (Not(Var"P"):&:Var"Q") == "((~P)&Q)"

--3 Definiti o functie eval care dat fiind o expresie logica si un mediu
--de evaluare, calculeaza valoarea de adevar a expresiei.

type Env = [(Nume, Bool)]

impureLookup ::Eq a=>a->[(a,b)]->b
impureLookup a=fromJust.lookup a

eval :: Prop -> Env -> Bool
eval (Var p) e = impureLookup p e
eval F e = False
eval T e = True
eval (Not p) e = not (eval p e)
eval (p :|: q) e = (eval p e) || (eval q e)
eval (p :&: q) e = (eval p e) && (eval q e)
eval (p :->: q) e = not(eval p e) || (eval q e)
eval (p :<->: q) e = (not(eval p e) || (eval q e)) && ((eval p e) || not(eval q e))

test_eval :: Bool
test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

--4. Definiti o functie care coreleaza lista tuturor variabilelor intr-o lista.

variabile :: Prop -> [Nume]
variabile T = []
variabile F = [] 
variabile (Var x) = [x]
variabile (Not p) = variabile p
variabile (p :|: q) = nub(variabile p ++ variabile q) -- ca sa nu fie duplicate
variabile(p :&: q) = nub(variabile p ++ variabile q)
variabile(p :->: q) = nub(variabile p ++ variabile q)
variabile(p :<->: q) = nub(variabile p ++ variabile q)

test_variabile :: Bool
test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]


--5. Data fiind o lista de nume, definiti toate atributele de valori
--de adevar posibile.
envs :: [Nume] -> [Env]
envs nume =
    let submultimi = subsequences nume
    in map mkEnv submultimi
  where
    mkEnv submultime = map(valuation submultime) nume

    valuation :: [Nume] -> Nume -> (Nume, Bool)
    valuation subm n
      | n `elem` subm = (n, True)
      | otherwise = (n, False)

-- siruri :: Int -> [[Bool]]
-- siruri 1 = [[True], [False]]
-- siruri n = [map (op [True]) x | x <- siruri (n-1)] ++ [map (op [False]) x | x <- siruri (n-1)]
--   where
--     op :: [Bool] -> [[Bool]] -> [[Bool]]
--     op a list = [a ++ x | x <- list]


oop :: [Bool] -> [[Bool]] -> [[Bool]]
oop a list = [a ++ x | x <- list]


-- envs_fara_subsequences :: [Nume] -> [Env]
-- envs_fara_subsequences nume = 
  
    
test_envs :: Bool
test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

--6. Definiti o functie care data fiind o propozitie verifica daca aceasta
--e satisfiabila.

satisfiabila :: Prop -> Bool
satisfiabila x = any (==True) (map (eval x) (envs (variabile x)))

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True

test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

--7. Determinati daca o propozitie este True pentru orice valori ale variabilelor.
valida :: Prop -> Bool
valida x = not (satisfiabila (Not x))

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False

test_valida2 = valida (Not (Var "P") :|: Var "Q") == False

--8. Tabelul de adevar
tabelAdevar :: Prop -> String
tabelAdevar x = addSpaces (concat (variabile x)) ++ show x ++ "\n"++  "-------------------\n"++(concat [a++"\n"| a <-transformare(rezultate x) ])

addSpaces :: String -> String
addSpaces [] = []
addSpaces (x:xs) = x : ' ' : addSpaces xs

transformare ::[Env] -> [String]
transformare x =map concat [map op y| y<-x]
  where
    op (a,b)
      | b==True = "T "
      | otherwise = "F "


rezultate:: Prop -> [Env]
rezultate x= map op (envs (variabile x))
  where 
  op :: Env -> Env
  op a = a ++ [(convertStringToName( showProp x), eval x a)]

convertStringToName :: String -> Nume
convertStringToName x = x


--pt 9 am adaugat la declararea tipului si la functiile: variabile, showProp si eval
--10. Testare daca 2 propozitii sunt echivalente.

echivalenta :: Prop -> Prop -> Bool
echivalenta x y = transformare(rezultate x) == transformare(rezultate y)

test_echivalenta1 :: Bool
test_echivalenta1 = True == (Var"P":&:Var"Q")`echivalenta`(Not(Not(Var"P"):|:Not(Var"Q")))

test_echivalenta2 :: Bool
test_echivalenta2 = False == (Var"P")`echivalenta`(Var"Q")

test_echivalenta3 :: Bool
test_echivalenta3 = True == (Var"R":|:Not(Var"R"))`echivalenta`(Var"Q":|:Not(Var"Q"))