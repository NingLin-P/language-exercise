module  ChurchNumbers  where

import Data.Function
import Data.List
import Data.Char
import Data.Ord
import Control.Monad
import Data.Maybe
import Control.Applicative


-- Church Numbers - Add, Multiply, Exponents

type Lambda a = (a -> a)
type Cnum a = Lambda a -> Lambda a

zero :: Cnum a
zero f = id

churchSucc :: Cnum a -> Cnum a
churchSucc c = (\h -> h . c h)

churchify 0 = zero
churchify n = churchSucc (churchify (n-1))

numerify :: Cnum Int -> Int
numerify c = c (+1) 0

churchAdd :: Cnum a -> Cnum a -> Cnum a
churchAdd c1 c2 f = c1 f . c2 f  -- liftA2 (.)


churchMul :: Cnum a -> Cnum a -> Cnum a
churchMul c1 c2 = c1 . c2


churchPow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
churchPow cb ce = ce cb