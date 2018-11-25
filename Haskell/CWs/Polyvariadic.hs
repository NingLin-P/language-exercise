{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module PolyvariadicFunctions where

class Poly u t where
    polyX :: u -> t

-- polyAdd
instance (t ~ Int ) =>  Poly Int t where
    polyX i = i

instance (u ~ Int , Poly u t) => Poly Int (u -> t) where
    polyX i x = polyX $ i + x

polyAdd :: (u ~ Int, Poly u t) => t
polyAdd = polyX (0::Int)

-- polyWords
instance (t ~ String) => Poly String t where
    polyX s = s

instance (u ~ String, Poly u t) => Poly String (u -> t) where
    polyX [] s2 = polyX s2
    polyX s1 s2 = polyX $ s1 ++ ' ':s2

polyWords :: (u ~ String, Poly u t) => t
polyWords = polyX ""

-- polyList
class PolyL u t where
    _polyList :: u -> t

instance (u ~ [a]) => PolyL u [a] where
    _polyList i = i

instance (u ~ [a], PolyL u t) => PolyL u (a -> t) where
    _polyList n i = _polyList (n ++ [i])

class PList t where
    polyList :: t

instance (PolyL [a] t) => PList (a -> t) where
    polyList = _polyList []

instance PList [a] where
    polyList = []
