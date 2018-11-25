{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair (SPair p) = p (,)
fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair (\f -> f a b)
fst :: SPair a b -> a
fst (SPair p) = p const
snd :: SPair a b -> b
snd (SPair p) = p (flip const)
swap :: SPair a b -> SPair b a
swap (SPair p) = SPair (p . flip)
curry :: (SPair a b -> c) -> (a -> b -> c)
curry fn a b = fn $ fromPair (a, b)
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry fn (SPair p) = p fn

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe const
fromMaybe (Just a) = SMaybe (\ _ f -> f a)
isJust :: SMaybe a -> Bool
isJust (SMaybe m) = m False (const True)
isNothing :: SMaybe a -> Bool
isNothing = not . isJust
catMaybes :: SList (SMaybe a) -> SList a
catMaybes = foldr fn (SList const)
    where fn m l = case toMaybe m of
                   Nothing -> l
                   (Just v) -> cons v l

toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right
fromEither :: Either a b -> SEither a b
fromEither (Left l) = SEither (\f _ -> f l)
fromEither (Right r) = SEither (\_ f -> f r)
isLeft :: SEither a b -> Bool
isLeft (SEither e) = e (const True) (const False)
isRight :: SEither a b -> Bool
isRight = not . isLeft
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = fromPair . foldr fn (SList const, SList const)
    where fn e (l,r) = case toEither e of
                       (Left v) -> (cons v l,r)
                       (Right v) -> (l, cons v r)
 
toList :: SList a -> [a]
toList = foldr (:) []
fromList :: [a] -> SList a
fromList [] = SList const
fromList (n:ns) = SList (\_ f -> f n (fromList ns))
cons :: a -> SList a -> SList a
cons a slist = SList (\_ f -> f a slist)
concat :: SList a -> SList a -> SList a
concat slist@(SList list) l2 = 
    if null slist 
    then l2 
    else SList (\b f -> list b (\a res -> f a (concat res l2)))
null :: SList a -> Bool
null (SList list) = list True (\_ _ -> False)
length :: SList a -> Int
length = foldl (\n _ -> n + 1) 0
map :: (a -> b) -> SList a -> SList b
map g (SList list) = SList (\b f -> list b (\a res -> f (g a) (map g res)))
zip :: SList a -> SList b -> SList (SPair a b)
zip list1 list2 = map fromPair $ fromList $ h la lb
    where la = toList list1
          lb = toList list2
          h [] _ = []
          h _ [] = []
          h (x:xs) (y:ys) = (x,y): h xs ys
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl fn b (SList list) = list b (\a res -> foldl fn (fn b a) res)
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr fn b (SList list) = list b (\a res -> fn a (foldr fn b res))
take :: Int -> SList a -> SList a
take i (SList list) = SList (\b f -> list b (\a res -> if i <= 0 then b else f a (take (i-1) res)))
