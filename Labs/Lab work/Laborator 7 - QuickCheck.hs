module Lib where

import           Test.QuickCheck

-- stack new lab7
-- in package.yaml, sub base, - QuickCheck
-- stack ghci


someFunc :: IO ()
someFunc = putStrLn "someFunc"

double :: Int -> Int
double = undefined
triple :: Int -> Int
triple = undefined
penta :: Int -> Int
penta = undefined

test x = (double x + triple x) == (penta x)

myLookUp :: Int -> [(Int,String)]-> Maybe String
myLookUp n [] = Nothing
myLookUp n ( (key, val) : rest )
  | key == n = Just val
  | otherwise = myLookUp n rest

-- testati egalitatea cu lookup
testLookUp :: Int -> [(Int,String)] -> Bool
testLookUp n l = myLookUp n l == lookup n l

-- class Arbitrary a where
--      arbitrary :: Gen a
--
-- instance Arbitrary Int where
--      arbitrary = ... -- o sa fie de tipul Gen Int
--

-- Generatorul pentru Int va genera numere intregi, apoi numerele vor fi filtrate cu conditia data
testLookUpCond :: Int -> [(Int,String)] -> Property
testLookUpCond n list = n > 0 && n `div` 5 == 0 ==> testLookUp n list

-- Vrem un generator care sa genereze doar elemente cu conditia data
testLookUpCond' :: [(Int, String)] -> Property
testLookUpCond' list = forAll myGen (\n -> testLookUp n list)
  -- vedeti: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:forAll
  where
    myGen :: Gen Int
    myGen = choose (1, 4)
    -- vedeti: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:choose

data ElemIS = I Int | S String
     deriving (Show,Eq)

-- Int si String au instante de Arbitrary
instance Arbitrary ElemIS where

-- Implementarea 1:
instance Arbitrary ElemIS where
    -- arbitrary :: Gen ElemIS
    arbitrary =
        let iGen = fmap I (arbitrary :: Gen Int) -- nu e nevoie sa scrieti explicit tipul
            sGen = fmap S (arbitrary :: Gen String)
         in oneof [iGen, sGen]
-- Explicatii (Gen are instanta de Functor):
--   fmap :: (a -> b) -> f a -> f b
-- Aici, f ~ Gen si b ~ ElemIS, iar a ~ Int sau a ~ String.
-- Cand a ~ Int:
--   I :: Int -> ElemIS
--   fmap :: (Int -> ElemIS) -> Gen Int -> Gen ElemIS
--   fmap I :: Gen Int -> Gen ElemIS
--   arbitrary :: Gen Int
--   => fmap I arbitrary :: Gen ElemIS
-- Cand a ~ String:
--   S :: String -> ElemIS
--   fmap :: (String -> ElemIS) -> Gen String -> Gen ElemIS
--   fmap S :: Gen String -> Gen ElemIS
--   arbitrary :: Gen String
--   => fmap S arbitrary :: Gen ElemIS
--
-- oneof: vedeti https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:oneof

-- Implementarea 2:
-- instance Arbitrary ElemIS where
    -- arbitrary :: Gen ElemIS
    --
    -- Explicatie (Gen are instanta de Monad): putem folosi do-notation
    -- arbitrary = do
    --     intVal <- arbitrary :: Gen Int -- nu e nevoie sa scrieti explicit tipul
    --     strVal <- arbitrary :: Gen String
    --     elements [I intVal, S strVal]
-- Explicatii:
--   cu '<-' ne referim la valoarea generata de 'arbitrary';
--   'arbitrary' e o procedura/actiune care genereaza elemente arbitrare de tip 'a';
--   in partea stanga a lui '<-' va fi un element aleator de tip 'a';
--
-- elements: vedeti https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:elements

myLookUpElem :: Int -> [(Int,ElemIS)]-> Maybe ElemIS
myLookUpElem = undefined

testLookUpElem :: Int -> [(Int,ElemIS)] -> Bool
testLookUpElem = undefined
