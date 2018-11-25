module Snippet where

-- Using point free style , especially when writing a lambda function
-- Using helper function !!!

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)
-- there are also (....) = (.).(.).(.).(.)

fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap
-- there are also fmap3 = fmap . fmap . fmap