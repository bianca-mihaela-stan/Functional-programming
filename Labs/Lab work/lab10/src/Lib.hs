module Lib
    ( someFunc
      import Test.QuickCheck hiding (Failure, Success)
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

semigroupAssoc :: (Eq m, Semigroup m) => m->m->m->Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <>b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

--trivial
data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty  = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivId    = Trivial -> Bool

testTrivial :: IO ()
testTrivial
  = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: TrivId)
    quickCheck (monoidRightIdentity :: TrivId)


--identity
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool
type IdentityId    a = Identity a -> Bool

testIdentity :: IO ()
testIdentity
  = do
    quickCheck (semigroupAssoc :: IdentityAssoc String)
    quickCheck (monoidLeftIdentity :: IdentityId [Int])
    quickCheck (monoidRightIdentity :: IdentityId [Int])


--pair
data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where 
    Two x y <> Two z t = Two (x <> z) (y <> t) 

instance (Monoid a, Monoid b) => Monoid (Two a b) where 
    mempty = Two mempty mempty 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
type TwoId    a b = Two a b -> Bool

testTwo :: IO ()
testTwo
  = do
    quickCheck (semigroupAssoc :: TwoAssoc String [Int])
    quickCheck (monoidLeftIdentity :: TwoId [Int] String)
    quickCheck (monoidRightIdentity :: TwoId [Int] [Int])

--triple

data Three a b c = Three a b c
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         ) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary


--boolean conjunction
newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where 
    BoolConj a <> BoolConj b = BoolConj (a && b)

instance Monoid BoolConj where 
    mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjId    = BoolConj -> Bool

testBoolDisj :: IO ()
testBoolDisj
  = do
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConjId)
    quickCheck (monoidRightIdentity :: BoolConjId)


--boolean disjunction
newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where 
    BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Monoid BoolDisj where 
    mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjId    = BoolDisj -> Bool

testBoolDisj :: IO ()
testBoolDisj
  = do
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisjId)
    quickCheck (monoidRightIdentity :: BoolDisjId)


--or
data Or a b = Fst a | Snd b
  deriving (Eq, Show)

-- nu poate fi facuta instanta de monoid conform descrierii din laborator
-- e <> x = x si x <> e = x, dar nu tine indiferent de cum facem alegerea
 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

--lifting monoid to function

newtype Combine a b = Combine { unCombine :: a -> b }

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary
