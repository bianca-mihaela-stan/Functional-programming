module Lib1
where
import Test.QuickCheck hiding (Failure, Success)


someFunc :: IO ()
someFunc = putStrLn "someFunc"


--------------------LABORATORUL 10-----------------
semigroupAssoc :: (Eq m, Semigroup m) => m->m->m->Bool 
semigroupAssoc a b c = (a <> (b <> c)) == ((a<> b) <> c)

monoidLeftIdentity ::(Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity ::(Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a

--exemplu 1 : trivial
data Trivial = Trivial
    deriving (Eq, Show)

--implementarea de Semigroup peste Trivial
instance Semigroup Trivial where
    _ <> _ = Trivial

--implementarea de Monoid peste Trivial
instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivId = Trivial -> Bool


testTrivial :: IO()
testTrivial
    = do
        quickCheck  (semigroupAssoc :: TrivAssoc)
        quickCheck  (monoidLeftIdentity :: TrivId)
        quickCheck  (monoidRightIdentity :: TrivId)


    
--exercitiul 2 : identity

--aici facem newtype pentru ca....??
newtype Identity a = Identity a
    deriving (Eq, Show)

--implementarea operatiei
instance Semigroup a =>Semigroup (Identity a) where
    Identity x <> Identity y = Identity ( x <> y)

--implementarea elementului neutru
instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty= Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary 

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool
type IdentityId a = Identity a -> Bool

testIdentity :: IO()
testIdentity
    = do
        quickCheck  (semigroupAssoc :: IdentityAssoc String)
        quickCheck  (monoidLeftIdentity :: IdentityId [Int])
        quickCheck  (monoidRightIdentity :: IdentityId [Int])

--exercitiul 3 : pair

data Two a b = Two a b
    deriving (Eq, Show)

--implementarea operatiei; si a si b trebuie sa aiba structura 
--de Semigroup ca sa poate fi implementata operatia pe ele
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two x y <> Two z t = Two (x <> z) (y <> t)

--se ia elementul neutru de la fiecare dintre cele 2(de-aia si ele trebuie sa fie monoizi)
instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary
    
-- exercitiul 4 : triple

data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    Three x y z <> Three d e f = Three (x <> d) (y <> e) (z <> f)

--se ia elementul neutru de la fiecare dintre cele 2(de-aia si ele trebuie sa fie monoizi)
instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

--exercitiul 5 : boolean conjunction

--again, newtype pentru ca: avem si monid cu conjunctie si cu disjunctie pe Bool
newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj True

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> arbitrary

--exercitiul 6: boolean disjunction
newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Monoid BoolDisj where
    mempty = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> arbitrary

--exercitiul 7 : or
data Or a b = Fst a | Snd b
    deriving (Eq, Show)

-- instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
--     Or a b <> Or c d

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where 
--     arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

--exercitiul 8 : lifting monoid functions
newtype Combine a b = Combine {unCombine :: a -> b}

instance (CoArbitrary  a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary





