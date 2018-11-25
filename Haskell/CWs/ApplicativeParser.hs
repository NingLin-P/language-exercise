module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f (P p) = P (\str -> [(s, f a) | (s, a) <- p str])

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) a (P p) = P (\str -> [(s, a) | (s, _) <- p str])

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P pc
    where pc [] = []
          pc (c:cs) = [(cs, c) | p c]

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==) 

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P (\str -> [(str, x)])

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
(P pf) <@> (P px) = P (\str -> [ (str2, f v)  | (str1, f) <- pf str, (str2, v) <- px str1])

(<@) :: Parser a -> Parser b -> Parser a
(P pa) <@ (P pb) = P (\str -> [ (str2, a)  | (str1, a) <- pa str, (str2, _) <- pb str1])

(@>) :: Parser a -> Parser b -> Parser b
(P pa) @> (P pb) = P (\str -> [ (str2, b)  | (str1, _) <- pa str, (str2, b) <- pb str1])

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = inject []
stringP (c:cs) = (:) <#> charP c <@> stringP cs

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(P p1) <<>> (P p2) = P (\str -> p1 str ++ p2 str)

infixl 3 <<>>

-- think more about the what it do before write it
-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = inject [] <<>> some p

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> many p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser (P p) = map snd . filter (null.fst) . p

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p = go . runParser p
    where go [ans] = Just ans
          go _ = Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE i) = i
evalExpr (BinOpE AddBO e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (BinOpE MulBO e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (NegE e) = negate $ evalExpr e
evalExpr ZeroE = 0


-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique parseExprE

parseExprE :: Parser Expr
parseExprE = parseConstE <<>> parseBinOpExprE <<>> parseNegE <<>> parseZeroE

parseConstE :: Parser Expr
parseConstE = (ConstE . read) <#> some (predP isDigit)

parseBinOpExprE :: Parser Expr
parseBinOpExprE = (\e1 op e2 -> BinOpE op e1 e2) <#> 
                    (charP '(' @> parseExprE) <@> 
                    (charP ' ' @> parseBinOpE <@ charP ' ') <@> 
                    (parseExprE <@ charP ')')

parseBinOpE :: Parser BinOp
parseBinOpE = mapOp <#> (charP '+' <<>> charP '*')
    where mapOp '+' = AddBO
          mapOp '*' = MulBO

parseNegE :: Parser Expr
parseNegE = charP '-' @> (NegE <#> parseExprE)

parseZeroE :: Parser Expr
parseZeroE = charP 'z' @> inject ZeroE