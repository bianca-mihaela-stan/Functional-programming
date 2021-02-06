module Parsing where

import           Control.Applicative
import           Data.Char
import           Data.List           (intercalate)


-- -------------------------------------------------------------------------------------------
-- Mica introducere:
-- Tipul asta de parser se numeste parser combinator, si ne lasa sa scriem parsere
-- care se citesc usor, si care seamana mult cu structura pe care vrem sa o extragem
-- din textul dat ca input.
-- -------------------------------------------------------------------------------------------
--
-- Acum, vrem sa modelam parsarea. Putem reprezenta un parser ca fiind o functie
-- care ia un String, si intoarce mai multe rezultate, fiecare fiind reprezentat
-- de rezultatul efectiv al parsarii (reprezentarea structurata din limbaj)
-- si ce a mai ramas din String-ul primit ca input.
-- Deci, aici lista ne da posibilitatea de a avea mai multe rezultate, iar perechea
-- (a, String) are pe prima pozitie ceva rezultat din parsare, si pe a doua restul
-- String-ului de intrare.
-- Vrem sa avem mai multe rezultate, pentru ca parsarea poate sa fie ambigua: se poate ca acelasi
-- string sa fie "inteles" in doua sau mai multe feluri diferite.
newtype Parser a = Parser { apply :: String -> [(a, String)] }

-- Functia satisfy ne ofera un mod de a construi un parser care parseaza caractere
-- care satisfac un predicat primit ca input.
-- Spre exemplu, satisfy (== 'a') va returna un parser care recunoaste caracterul 'a'.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
  f []                 = []
  f (c:s) | p c        = [(c, s)]
          | otherwise = []

-- O sa folosim functia parse ca sa parsam efectiv un input. In principiu, va lua rolul lui runRandom
-- din exemplul anterior cu MyRandom. Observati ca parse chiar ruleaza apply (din definitia lui Parser),
-- dar mai face niste lucruri pe langa. Concret, consideram ca parsarea a esuat daca nu avem nicio parsare
-- (lista cu rezultate e vida) sau daca avem mai multe rezultate (parsare ambigua). Daca va uitati la
-- definitia lui parses din clauza where, vedeti ca intoarce toate rezultatele posibile pentru care tot
-- sirul de intrare a fost consumat.
-- Exemplu: rulati parse (satisfy (== 'a')) "a" in interpretor. Incercati sa rulati voi si alte exemple,
-- ca sa vedeti concret cum se comporta parsarea.
parse :: Show a => Parser a -> String -> a
parse m s = case parses of
    [] -> error "No valid parse."
    [a] -> a
    l -> error ("Ambiguity. Possible parses: \n\t" ++ intercalate "\n\t" (map show l))
  where
    parses = [ x | (x,t) <- apply m s, t == "" ]

-- Incercati voi sa intelegeti instantele de Functor si de Applicative.
-- List comprehensions ne ajuta mult sa scriem instantele astea intr-un mod cat mai clar.
instance Functor Parser where
  fmap f p =
    Parser
      (\s -> [(f a, r) | (a, r) <- apply p s])

instance Applicative Parser where
  pure a = Parser (\s -> [(a, s)])
  pf <*> pa =
      Parser
          (\s -> [(f a, r) | (f, rf) <- apply pf s
                           , (a, r) <- apply pa rf ])

-- Teoretic, Alternative e o alta clasa care reprezinta monoizi.
-- In timp ce clasa Monoid e pentru tipuri cu kind-ul *, Alternative e o clasa
-- care reprezinta monoizi pentru tipuri cu kind-ul * -> *.
-- In plus, avem constrangerea ca tipurile respective sa fie Applicative.
-- Alternative are niste legi care asigura ca instantele de Alternative se comporta
-- bine in relatie cu cele de Applicative.
-- In practica, Alternative ne ofera un mod de a modela alegeri/branching.
-- Deobicei empty e computatia care nu face nimic (care esueaza), si prin pa <|> pb intelegem
-- "computatia pa sau computatia pb".
instance Alternative Parser where
  -- empty :: Parser a
  -- deci, in cazul nostru, e parserul care nu parseaza nimic din stringul de intrare
  -- (parserul care esueaza mereu)
  empty = Parser (\s -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  -- o sa incerce parserul pa si o sa incerce si parserul pb;
  -- astfel, putem sa zicem ca ceva se parseaza ori cu parserul pa ori cu parserul pb;
  -- o sa luam ambele variante in considerare, deci o sa facem branching si continuam parsarea
  -- pe ambele cai; bineinteles, una din cai poate sa esueze la un moment dat, si in final, un string
  -- ne-ambiguu va avea o singura parsare corecta
  pa <|> pb = Parser (\s -> apply pa s ++ apply pb s)
-- In plus, Alternative ne da functiile some si many:
--   some :: Alternative f => f a -> f [a]
--   many :: Alternative f => f a -> f [a]
-- Some inseamna "una sau mai multe", iar many "zero sau mai multe".
-- (Vedeti parserul char de mai jos) Un exemplu ar fi some (char 'a'), care ne va da parserul care
-- parseaza un caracter 'a' sau mai multe.

-- Incercati voi sa va intelegeti implementarile de mai jos, uitandu-va la tipuri si folosindu-le
-- in interpretor impreuna cu functia parse si un string de intrare.

-- Recunoasterea unui anumit caracter
char :: Char -> Parser Char
char c = satisfy (== c)

-- Folosim many ca sa parsam oricate spatii, apoi nu ne intereseaza rezultatul deci intoarcem unit ( () ).
-- Cautati voi pe Hoogle (*>) si incercati sa intelegeti ce face, lucrand si cu cateva exemple.
skipSpace :: Parser ()
skipSpace = many (satisfy isSpace) *> pure ()

-- Defineste un parser care, pe langa faptul ca parseaza ceva, ignora spatiile din jur.
token :: Parser a -> Parser a
token p = skipSpace *> p <* skipSpace

-- Mai stiti functia read? O aplicam ca transformare pe ce intoarce parserul din dreapta lui <$>.
-- Tineti minte ca <$> e un operator care face acelasi lucru ca fmap.
parseNat :: Parser Int
parseNat = read <$> some (satisfy isDigit)

-- Recunoasterea unui numar negativ
parseNeg :: Parser Int
parseNeg =  char '-' *> (negate <$> parseNat)

-- Recunoasterea unui numar intreg
-- Deci, parsam un numar intreg ori ca numar natural ori ca numar negativ.
-- E destul de clar ca pentru un numar intreg, unul din cele doua parsare o sa esueze,
-- dar continuam executia pe branch-ul bun.
parseInt :: Parser Int
parseInt = parseNat <|> parseNeg
