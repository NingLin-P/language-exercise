module Exer12 where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap g (Node l a r) = Node (fmap g l) (g a) (fmap g r)


instance Functor ((->) a) where
    -- famp :: (b -> c) -> (a -> b) -> (a -> c)
    fmap = (.)

instance Applicative ((->) a) where
    -- pure :: b -> (a -> b)
    pure = const

    -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
    g <*> h = \x -> g x (h x)

instance Monad ((->) a) where
    -- return :: b -> (a -> b)
    return = pure

    -- >>= :: (a -> b) -> (b -> a -> c) -> (a -> c)
    ms >>= f = f . ms


newtype ZipList a = Z [a] deriving Show 

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b 
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where 
    -- pure :: a -> ZipList a 
    pure x = Z (repeat x)
    
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b 
    (Z gs) <*> (Z xs) = Z [ g x | (ig , g) <- zip [1..] gs, (ix, x) <- zip [1..] xs, ig == ix]
    -- Better: (Z gs) <$> (Z xs) = Z [ g x | (g, x) <- zip gs xs]


data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap _ (Val v) = Val v
    fmap g (Var a) = Var (g a)
    fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    Var g <*> ms = fmap g ms
    