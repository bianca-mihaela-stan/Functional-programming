--Stan Bianca-Mihaela, grupa 232
main = print (partida1 Piatra Piatra, partida2 Hartie Foarfeca)

data Alegere
  = Piatra
  | Foarfeca
  | Hartie
  deriving (Eq, Show)

instance Ord Alegere where
    Piatra <= Hartie = True
    Hartie <= Foarfeca = True
    Foarfeca <= Piatra = True


data Rezultat
    = Victorie
    | Infrangere
    | Egalitate
  deriving (Eq, Show)

instance Ord Rezultat where
    Victorie <= Infrangere = True
    Infrangere <= Egalitate = True
    Egalitate <= Victorie = True

--am facut 2 functii partida, ca sa ma foloses de sintaxa din Haskell in cat mai multe moduri pentru a scrie o functie
partida1 :: Alegere -> Alegere -> Rezultat
partida1 x y 
    | x==Piatra && y==Foarfeca || x==Foarfeca && y==Hartie || x==Hartie && y==Piatra = Victorie
    | x==Piatra && y==Piatra || x==Foarfeca && y==Foarfeca || x==Hartie && y==Hartie = Egalitate
    | x==Piatra && y==Hartie || x==Foarfeca && y==Piatra || x==Hartie && y==Foarfeca = Infrangere

partida2 :: Alegere -> Alegere -> Rezultat
partida2 x y = 
    if x==Piatra && y==Piatra
        then Egalitate
        else if x==Piatra && y==Foarfeca
            then Victorie
            else if x==Piatra && y==Hartie
                then Infrangere
                else if x==Foarfeca && y==Piatra
                    then Infrangere
                    else if x==Foarfeca && y==Foarfeca
                        then Egalitate
                        else if x==Foarfeca && y==Hartie
                            then Victorie
                            else if x==Hartie && y==Piatra
                                then Victorie
                                else if x==Hartie && y==Foarfeca
                                    then Infrangere
                                    else Egalitate
