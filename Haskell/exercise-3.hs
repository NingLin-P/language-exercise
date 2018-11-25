module Exer3 where


doubleMe :: Int -> Int
doubleMe x = x + x

-- (+++) :: [a] -> [a] -> [a]
[]+++ ys = ys 
(x:xs) +++ ys = x : (xs +++ ys)

drop' 0 nx = nx
drop' _ [] = []
drop' n (_:nx) = drop' (n - 1) nx

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

product' :: Num a => [a] -> a 
product' = foldl (*) 1

init' :: [a] -> [a]
init' [_] = []
init' (x:nx) = x: init' nx

reverse' :: [a] -> [a]
reverse' = foldr (\x n -> n ++ [x]) []

data Loob = Talse | Frue deriving Show
data ArTy = Ta Int | Tb Char deriving Show
fun x = Frue
func x y = Ta (x+y) 


act :: IO (Char,Char) 
act = do 
    x <- getChar 
    getChar 
    y <- getChar 
    return (x,y)

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

pairs xs ys zs = do 
    x <- xs 
    y <- ys 
    z <- zs
    return (x,y,z)
