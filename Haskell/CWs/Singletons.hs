{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (a :: Nat) :-: (b :: Nat) :: Nat
type instance Zero :-: m = Zero
type instance n :-: Zero = n
type instance (Succ n) :-: (Succ m) = n :-: m

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero m = m
type instance Add (Succ n) m = Succ (Add n m)

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min Zero m = Zero
type instance Min n Zero = Zero
type instance Min (Succ n) (Succ m) = Succ (Min n m)

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons v _) = v
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate s (SSucc n) = VCons s (replicate s n) 

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a l -> Vec b l -> Vec c l 
zipWith _ VNil VNil = VNil
zipWith fn (VCons a xs) (VCons b ys) = VCons (fn a b) (zipWith fn xs ys)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ b = b
(VCons v ns) ++ b = VCons v (ns ++ b) 

-- The semantics should match that of take for normal lists.
take :: (c ~ (Min a b)) => SNat a -> Vec s b -> Vec s c
take SZero _ = VNil
take _ VNil = VNil
take (SSucc n) (VCons s ns) = VCons s (take n ns)

-- The semantics should match that of drop for normal lists.
drop :: (c ~ (b :-: a)) => SNat a -> Vec s b -> Vec s c
drop SZero ns = ns
drop _ VNil = VNil
drop (SSucc n) (VCons _ ns) = drop n ns

head :: Vec s (Succ n) -> s
head (VCons s _) = s 

tail :: Vec s (Succ n) -> Vec s n
tail (VCons _ n) = n
