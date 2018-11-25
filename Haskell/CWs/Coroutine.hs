{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Coroutine where
import Control.Monad (ap, forever)

newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)

data Command r u d a =
    Done a
  | Out d (Coroutine r u d a)
  | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

fromComd :: Command r u d a -> Coroutine r u d a
fromComd comd = Coroutine ($ comd)

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return x = Coroutine (\k -> k (Done x))
  (Coroutine c) >>= g  = Coroutine (c . fn)
    where fn k (Done a)     = apply (g a) k
          fn k (Out v cont) = k (Out v (cont >>= g))
          fn k (In f)       = k (In (\v -> f v >>= g))

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
(Coroutine pB) >>> (Coroutine pA) = Coroutine (\k -> pA (stepA k pB))
  where stepA k n (Done a)     = k (Done a)
        stepA k n (Out d cont) = k (Out d $ Coroutine n >>> cont)
        stepA k n (In fm)      = n (stepB k fm)
        stepB k f (Done a)     = k (Done a)
        stepB k f (In fu)      = k (In (\u -> fu u >>> fromComd (In f)))
        stepB k f (Out m cont) = apply (f m) (stepA k (apply cont))

-- It might be useful to define the following function
pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
pipe2 = undefined

-- Library functions

output :: a -> Coroutine r u a ()
output v = fromComd (Out v (return ()))

input :: Coroutine r v d v
input = fromComd (In return)

produce :: [a] -> Coroutine r u a ()
produce [] = return ()
produce (x:xs) = fromComd (Out x (produce xs))

consume :: Coroutine [t] u t a -> [t]
consume c = apply c fn
    where fn (Out v cont) = v : consume cont
          fn (Done _) = []

inputFn :: (u -> (Command r u d a -> r) -> r) -> Coroutine r u d a
inputFn fn = fromComd (In (Coroutine . fn))

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = inputFn (\v k -> if p v then k (Out v (filterC p)) else apply (filterC p) k)

limit :: Int -> Coroutine r v v ()
limit n = inputFn (\v k -> if n > 0 then k (Out v (limit (n-1))) else k (Done ()))

suppress :: Int -> Coroutine r v v ()
suppress n = inputFn (\v k -> if n <= 0 then k (Out v (suppress n)) else apply (suppress (n-1)) k)

add :: Coroutine r Int Int ()
add = forever $ do
    a <- input
    b <- input
    output (a+b)


duplicate :: Coroutine r v v ()
duplicate = forever $ do
        i <- input
        output i
        output i

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = p2Stream 1
p2Stream n = Coroutine (\k -> k (Out (n*(n+1)`div`2) (p2Stream (n + 1))))
p3 = duplicate >>> add
p4 = duplicate >>> suppress 1 >>> add
