{-# LANGUAGE DeriveFunctor #-}

module OperatorParser
    ( OpTree(..)
    , Associativity(..)
    , op
    , foldTree
    , parseOperators
    , module Text.ParserCombinators.ReadP
    )
where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Data.Char
-- import Text.Parsec


-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b = Op (OpTree a b) a (OpTree a b)
                | Term b
                deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
                     deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree _ (Term b) = b
foldTree f (Op b1 oper b2) = f oper (foldTree f b1) (foldTree f b2)

-- | Return a parser such that: given 'op s a', if s matches, the parser 
-- | returns a.
op :: String -> a -> ReadP a
op s a = do _ <- string s
            return a

-- | Accept two arguments: 
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity; 
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree.

token :: ReadP a -> ReadP a
token pa = do skipMany $ satisfy isSpace
              p <- pa
              skipMany $ satisfy isSpace
              return p

parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators ops = parseOp ops ops

parseOp :: [Associativity [ReadP a]] -> [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOp allop ops@(R ps:xs) tp = 
    do t1 <- parseOp allop xs tp
       (<|>) (do { p <- token $ foldl1 (+++) ps;
                   t2 <- parseOp allop ops tp;
                   return $ Op t1 p t2; })
             (return t1)

parseOp allop (NoAssociativity ps:xs) tp = 
    do t1 <- parseOp allop xs tp
       (<|>) (do { p <- token $ foldl1 (+++) ps;
                   t2 <- parseOp allop xs tp;
                   return $ Op t1 p t2; })
             (return t1)

parseOp allop ops@(L _:xs) tp = 
    do t <- parseOp allop xs tp
       helper t ops tp
    where helper t1 ops@(L ps:xs) tp =
            (<|>) (do { p <- token $ foldl1 (+++) ps;
                        t2 <- parseOp allop xs tp;
                        helper (Op t1 p t2) ops tp; })
                  (return t1)

parseOp allop [] tp = 
    (<|>) (do { _ <- char '(';
                t <- token $ parseOp allop allop tp;
                _ <- char ')';
                return t; })
          (Term <$> tp )


brackets :: OpTree String String -> String
brackets = foldTree (\o x y -> '(' : x ++ o ++ y ++ ")")

arithOps :: [Associativity [ReadP String]]
arithOps =
    map (fmap (map (\s -> op s s) . words))
    [ R "&& ||", NoAssociativity "< > =", L "+ -", L "* /", R "^"]

arithParser :: ReadP (OpTree String String)
arithParser = parseOperators arithOps (munch1 isDigit) <* eof

parse str = map (brackets . fst) $ filter (null . snd) (readP_to_S arithParser str)