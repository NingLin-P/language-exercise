module PolynomField where

import Data.List

type Power = Int
data BinaryPolynom = PNil | PCons Power BinaryPolynom deriving (Eq)

zero, one :: BinaryPolynom
zero = PNil
one  = PCons 0 PNil

deg :: BinaryPolynom -> Int
deg PNil = -1
deg (PCons i _) = i

-- | Constructs a monom with the given degree.
polyFromDeg :: Int -> BinaryPolynom
polyFromDeg (-1) = PNil
polyFromDeg i = PCons i PNil

polyFromPowers :: [Int] -> BinaryPolynom
polyFromPowers = foldr PCons PNil . reverse . sort

instance Show BinaryPolynom where
    show PNil = "0"
    show (PCons i PNil) = "x^" ++ show i
    show (PCons i n) = "x^" ++ show i ++ " + " ++ show n

mul' :: BinaryPolynom -> Int -> BinaryPolynom
mul' PNil _ = PNil
mul' (PCons i pn) n = PCons (i+n) $ mul' pn n

-- | Multiplication in the polynom ring.
multiply :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
multiply _ PNil = PNil
multiply x (PCons j yn) = mul' x j .+. multiply x yn

-- | Addition and multiplication in the polynom field.
(.+.), (.*.) :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
x .+. PNil = x
PNil .+. y = y
x@(PCons i xn) .+. y@(PCons j yn) | i == j = xn .+. yn
                                  | i > j = PCons i (xn .+. y)
                                  | otherwise = PCons j (x .+. yn)

x .*. y = snd $ polyDivMod (x `multiply` y) (polyFromPowers [8,4,3,1,0])


-- polyDivMod :: BinaryPolynom -> BinaryPolynom -> (BinaryPolynom, BinaryPolynom)
polyDivMod a b = go a b []
    where go PNil _ ans = (polyFromPowers ans, zero)
          go x@(PCons i _) y@(PCons j _) ans | i < j = (polyFromPowers ans, x)
                                             | otherwise = go (x .+. (mul' y (i-j))) y ((i-j):ans)