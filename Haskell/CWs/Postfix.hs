{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Postfix where

data Add = Add
data Push = Push
data End = End

class Postfix t where
    postFix :: [Int] -> t

instance (u ~ Int, Postfix t) => Postfix (Push -> u -> t) where
    postFix ns Push i = postFix (i:ns)

instance (Postfix t) => Postfix (Add -> t) where
    postFix (x:y:ns) Add = postFix (x+y:ns)

instance (t ~ Int) => Postfix (End -> t) where
    postFix (x:_) End = x

push  = Push
add   = Add
end   = End

begin :: (Postfix t) => t
begin = postFix []

-- better solution :

-- begin = ($ [])

-- push s = \i f -> f (i:s)

-- end = head

-- add (a:b:ss) = \f -> f (a+b:ss)