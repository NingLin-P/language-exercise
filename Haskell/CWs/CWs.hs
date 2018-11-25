module CWs where

import Data.Function
import Control.Applicative
import Data.List
import Data.Char
import Data.Ord
import Control.Monad
import Data.Maybe
import Data.Foldable (foldMap, Foldable)
import Data.Monoid

-- digitalRoot
digitalRoot :: Integral a => a -> a
digitalRoot x
    | x < 10 = x
    | otherwise = digitalRoot (split x)


split :: Integral a => a -> a
split x
    | x < 10 = x
    | otherwise = x `mod` 10 + split (div x 10)


-- Disemvowel Trolls
disemvowel :: String -> String
disemvowel = filter (`notElem` "aeiouAEIOU")
-- disemvowel ns = [x | x <- ns , not (elem x "aeiouAEIOU")]


solution :: Integer -> Integer
solution num = sum [ x | x <- [3..(num-1)],  x `mod` 3 == 0 || x `mod` 5 == 0 ]


-- Find The Parity Outlier

findOutlier :: [Int] -> Int
findOutlier ns@(x:y:z:_) = helper ns
    where isodd = ((<2) . length . filter odd) [x,y,z]
          helper (n:xs) | (odd n == isodd) = n | otherwise = helper xs


-- Find the missing letter
findMissingLetter :: [Char] -> Char
findMissingLetter cs = helper pairs
    where
        pairs = zip cs (tail cs)
        helper [] = error "no missing letter"
        helper (n:xs) | (ord $ snd n) - (ord $ fst n) /= 1 = chr $ (+1) $ ord $ fst n
                      | otherwise = helper xs
-- Hint: use (\\) or succ


-- Remove the minimum
removeSmallest :: [Int] -> [Int]
removeSmallest [] = []
removeSmallest xs = delete (minimum xs) xs
-- removeSmallest = delete =<< minimum    


-- Valid Braces
validBraces :: String -> Bool
validBraces = helper []
    where
        helper stack [] = null stack
        helper stack (x:xs) | null stack = helper [x] xs
                            | elem x ")]}" = macth (last stack) x && helper (init stack) xs
                            | otherwise = helper (stack ++ [x]) xs

macth :: Char -> Char -> Bool
macth '(' ')' = True
macth '[' ']' = True
macth '{' '}' = True
macth _ _ = False

-- Better solution , use pattern matching instead macth function
validBraces' :: String -> Bool
validBraces' s = "" == foldr collapse [] s

collapse '(' (')':xs) = xs
collapse '{' ('}':xs) = xs
collapse '[' (']':xs) = xs
collapse x xs = x:xs


-- Shortest Word
findShortest :: String -> Integer
findShortest = toInteger . minimum . (map length) . words

-- Your order, please
yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy (\x y -> if (getOrd x) > (getOrd y) then GT else LT) . words
getOrd :: [Char] -> Int
getOrd (s:sn) | isDigit s = digitToInt s | otherwise = getOrd sn

-- listSquared 
listSquared :: Int -> Int -> [(Int, Int)]
listSquared n m = reverse $ tail $ foldr getAns [] [n..m]

getAns y [] = getAns y [(1,0)]
getAns y ns@(x:_) = let next = sumDivs x y in if isSquared next then ns ++ [(y, next)] else ns

sumDivs f n = (snd f) + sum [ i^2 | i <- [(fst f)..n], n `mod` i == 0]

isSquared n = isInt $ sqrt $ fromIntegral n
        where isInt n = floor n == ceiling n


-- Find the missing term in an Arithmetic Progression
findMissing :: Integral n => [n] -> n
findMissing ns@(x:y:zs) = if null ans then findMissing $ reverse ns else head ans
                    where ans = [x,y .. last zs] \\ ns

-- Detect Pangram
isPangram :: String -> Bool
isPangram str = null $ ['a'..'z'] \\ map toLower str


-- Counting Duplicates
countDup :: String -> Int
countDup = length . filter ( (/=1).length ) . group . sort . map toLower


-- sortArray
sortArray :: [Int] -> [Int]
sortArray ns = helper (sort $ filter odd ns) ns
    where
        helper [] ns = ns
        helper odds [] = odds
        helper odds@(o:rest) (n:ns) | odd n = o: helper rest ns
                                    | otherwise = n: helper odds ns


-- -- Reverse or rotate?
revrot :: String -> Int -> String
revrot str sz = concatMap helper $ splitSz str sz
        where helper xs | isIt xs = reverse xs | otherwise = tail xs ++ [head xs]

isIt = (==0) . (`mod` 2) . sum . map ( (^2) . digitToInt )

splitSz str sz | length str < sz || sz <= 0 = []
               | otherwise = fst (splitAt sz str) : splitSz (snd $ splitAt sz str) sz



-- Directions Reduction
data Direction = North | East | West | South deriving (Eq, Show)

dirReduce :: [Direction] -> [Direction]
dirReduce = reverse . foldl redu []

redu (South:ds) North = ds
redu (North:ds) South = ds
redu (East:ds) West = ds
redu (West:ds) East = ds
redu ds d = d:ds


-- Perimeter of squares in a rectangle
perimeter :: Integer -> Integer
perimeter  = (*4) . sum . (`take` fibs) . fromIntegral . (+1)
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)



-- Weight for weight
orderWeight :: [Char] -> [Char]
orderWeight = unwords . sortBy ordW . words

ordW x y | cop x y == EQ = compare x y
         | otherwise = cop x y
    where
        cop = compare `on` (sum . map digitToInt)
-- use `on` comparing


-- Next smaller number with the same digits
-- solution: find tow position, swicth them and sort the left side
nextSmaller :: Integer -> Maybe Integer
nextSmaller num | sortStr cs = Nothing
                | otherwise = Just (read ans :: Integer)
        where
            cs = show num
            indexCs = zip [1..] (reverse cs)
            go (i, n) = map (\(j, _) -> [j,i]) $ filter (\(_, v) -> v > n) (drop i indexCs)
            [x,y] = map (length cs -) $ head $ sortOn (!!0) $ concatMap go indexCs
            ans = take x cs ++ [cs !! y] ++ sortOn Down (delete (cs !! y) $ drop x cs)

sortStr str | length str < 2 = True
            | str !! 1 == '0' = sortStr $ delete '0' str
            | otherwise = sort str == str


-- Human readable duration format
formatDuration :: (Show i , Integral i) => i -> String
formatDuration  = form . map (\(t, c) -> show t ++" "++ if t == 1 then init c else c)
                  . filter (\x -> fst x /= 0) . (`zip` comps) . go timeUnit
    where
        comps = ["years", "days", "hours", "minutes", "seconds"]
        timeUnit = [31536000, 86400, 3600, 60, 1]
        go [] _ = []
        go (x:xs) n = n `div` x : go xs (mod n x)
        form [] = "now"
        form [x] = x
        form [x,y] = x ++ " and " ++ y
        form xs@(x:_:_:_) = x ++ ", " ++ form (tail xs)


-- Hamming Numbers
hamming  :: Int -> Int
hamming = (!!) ham . (+ (-1))
        where
            ham = 1 : merg ( merg (map (*2) ham) (map (*3) ham) ) (map (*5) ham)
            merg (x:xs) (y:ys)  | x > y = y : merg (x:xs) ys
                                | x < y = x : merg xs (y:ys)
                                | otherwise = x : merg xs ys


-- Total increasing or decreasing numbers up to a power of 10
totalIncDec :: Integer -> Integer
totalIncDec 0 = 1
totalIncDec 1 = 10
totalIncDec x = helper [1..10] (x - 1)
    where
        flist = reverse . foldl (\xs v -> if null xs then [v] else head xs + v : xs) []
        getSum xs = ((+ head xs) $ (+ last xs) $ (*2) $ sum $ init xs) - 11
        helper _ 0 = 10
        helper xs n = getSum xs + helper (flist xs) (n - 1)



-- Sort binary tree by levels
data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels Nothing = []
treeByLevels (Just t) = go [t]
    where
        -- use catMaybes
        getS (TreeNode (Just l) (Just r) _) = [l,r]
        getS (TreeNode (Just l) Nothing _) = [l]
        getS (TreeNode Nothing (Just r) _) = [r]
        getS _ = []
        go [] = []
        go ts = map value ts ++ go (concatMap getS ts)
-- better solution: 
-- treeByLevels :: Maybe (TreeNode a) -> [a]
-- treeByLevels = concat . takeWhile (not . null) . go
--     where
--         go Nothing = repeat []
--         go (Just x) = [value x] : zipWith (++) (go $ left x) (go $ right x)



-- Last digits of N^2 == N
green:: Int -> Integer
green n = ([1,5,6] ++ (lmap fst `merg` lmap snd)) !! (n-1)
    where
        toInt x = read x :: Integer
        lmap = map toInt . filter ((/='0').head) . (`map` go ("5","6"))
        merg (x:xs) (y:ys)  | x < y = x : merg xs (y:ys)
                            | otherwise = y : merg (x:xs) ys

go (x,y) = (a:x , b:y) : go (a:x , b:y)
    where
        isGreen x = x `isSuffixOf` show ((^2) $ read x :: Integer)
        a = head [ a | a <- "1234567890" , isGreen (a:x) ]
        b = intToDigit (abs (digitToInt a - 9))


-- Calculator
data Token = A | S | M | D | V Double

evaluate :: String -> Double
evaluate = calc id . map tk . words where
    tk s = case s of "+" -> A; "-" -> S; "*" -> M; "/" -> D; v -> V (read v)
    calc _ [] = 0
    calc f [V v] = f v
    calc f (V v : A : t) = calc (f v +) t
    calc f (V v : S : t) = calc (f v -) t
    calc f (V v : M : V u : t) = calc f (V (v * u) : t)
    calc f (V v : D : V u : t) = calc f (V (v / u) : t)


-- Fix it
reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' _ [] = []
reverse' f (x:xs) = f xs ++ [x]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' _ _ b [] = b
foldr' rec f b (x:xs) = x `f` rec f b xs

fixedReverse = fix reverse'

fixedFoldr = fix foldr'

-- foldMap all the things!
data Mymin a = Mym a | Emp deriving (Eq, Ord)

instance (Ord a) => Semigroup (Mymin a) where
    a <> Emp = a
    Emp <> a = a
    a <> b = if a > b then b else a

instance (Ord a) => Monoid (Mymin a) where
    mempty = Emp
    mappend a b = if a > b then b else a

myToList :: Foldable t => t a -> [a]
myToList = foldMap (:[])

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum t = case foldMap Mym t of
                   Emp -> Nothing
                   (Mym a) -> Just a

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f b list = appEndo (foldMap (Endo . f) list ) b