{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (\aaa b1 b2 -> ab $ aaa (ba b1) (ba b2), \bbb a1 a2 -> ba $ bbb (ab a1) (ab a2))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
    zero = O
    successor = S
    nat a _ O = a
    nat _ fn (S p) = fn p
    iter a _ O = a
    iter a fn (S n) = iter (fn a) fn n
    plus O p = p
    plus (S n) p = S (plus n p)
    minus O _ = O
    minus n O = n
    minus (S a) (S b) = minus a b
    mult O _ = O
    mult (S a) n = n `plus` mult a n
    pow _ O = 1 -- because Nat is an instance of Num
    pow n (S a) = n `mult` pow n a


-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero = []
  successor = (():)
  nat a _ [] = a
  nat _ fn (_:xs) = fn xs
  iter a _ [] = a
  iter a fn (_:xs) = iter (fn a) fn xs 
  plus = (++)
  minus [] _ = []
  minus n [] = n
  minus (_:xs) (_:ys) = minus xs ys
  mult [] _ = []
  mult (_:xs) ys = ys `plus` mult xs ys
  pow _ [] = [()]
  pow n (_:xs) = n `mult` pow n xs


-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  zero = Scott const
  successor sco = Scott (\_ fn -> fn sco)
  nat a fn (Scott sco) = sco a fn
  iter a fn (Scott sco) = sco a (iter (fn a) fn)
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
isZero :: Church -> Bool
isZero chur = runChurch chur (const False) True
instance Nat Church where
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.
  zero = Church (flip const)
  successor (Church chur) = Church (\fn -> fn . chur fn)
  nat a fn chur = if isZero chur then a else fn (chur `minus` successor zero)
  iter a fn (Church chur) = chur fn a
  plus (Church c1) (Church c2) = Church (\fn -> c1 fn . c2 fn)
  minus = substR (liftISO2 isoP) minus
  mult (Church c1) (Church c2) = Church (c1 . c2)
  pow (Church c1) (Church c2) = Church (c2 c1)
