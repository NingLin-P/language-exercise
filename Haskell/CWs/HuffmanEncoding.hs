module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import Data.List

data Bit = Z | O deriving (Eq, Show)

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = map (\xs -> (head xs, length xs)) . group . sort


-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode [] _ = Nothing
encode [_] _ = Nothing
encode freq l = concat <$> mapM (`lookup` mapping) l
    where
        mapping = getMapping freq


-- | Decode a bit sequence using the given frequencies.
decode :: [(a, Int)] -> [Bit] -> Maybe [a]
decode [] _ = Nothing
decode [_] _ = Nothing
decode freq codes = sequence $ go codes
    where
        mapping = getMapping freq
        go [] = []
        go cs = case find (\(_,t) -> t `isPrefixOf` cs) mapping of
                  Nothing -> [Nothing]
                  Just (c, t) -> Just c : go (drop (length t) cs)

getMapping freq = mkTree initList
    where
        initList = map (\(a, n) -> ([(a,[])], n)) freq
        addToken (ns,w) token = (map (\(a, ts) -> (a, token:ts)) ns, w)
        merg (x, wx) (y, wy) = (x ++ y, wx + wy)
        mkTree [(ans,_)] = ans
        mkTree xs@(_:_:_) = let r:l:ns = sortOn snd xs
                            in mkTree $ merg (addToken r Z) (addToken l O) : ns


