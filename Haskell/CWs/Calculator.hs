module Calculator where

import Data.Char
import Control.Applicative



newtype Parser a = P (String -> [(a, String)])

parser :: Parser a -> String -> [(a , String)]
parser (P p) = p

item :: Parser Char
item = P (\s -> case s of
                [] -> []
                xs -> [(last xs, init xs)])


instance Functor Parser where
    fmap f p = P (\s -> case parser p s of
                        [] -> []
                        [(a, s')] -> [(f a, s')])

instance Applicative Parser where
    pure a = P (\s -> [(a, s)])
    (P g) <*> px = P (\s -> case g s of
                            [] -> []
                            [(f, s')] -> parser (fmap f px) s')

instance Monad Parser where
    return = pure
    (P p) >>= f = P (\s -> case p s of
                            [] -> []
                            [(a, s')] -> parser (f a) s')


three :: Parser (Char, Char)
three = do
    x <- item
    item
    y <- item
    return (x,y)


instance Alternative Parser where
    empty = P $ const []
    (P p) <|> (P q) = P (\s -> case p s of
                                [] -> q s
                                [(a, s')] -> [(a, s')])


sat :: (Char -> Bool) -> Parser Char
sat p =
    do x <- item
       if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower = sat isLower

upper = sat isUpper

letter = sat isAlpha

alphanum = sat isAlphaNum

char c = sat (== c)

string [] = return []
string (x:xs) = 
    do char x
       string xs
       return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)


nat :: Parser Double
nat = do xs <- some digit
         return (read $ reverse xs)

flo :: Parser Double
flo = do xs <- some digit
         char '.'
         ys <- some digit
         return $ read $ reverse $ xs++"."++ys
      <|> nat

real :: Parser Double
real = do char '-'
          f <- flo
          return (-f)
       <|> flo

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier = token ident

natural = token nat

realnum = token real

symbol xs = token (string xs)

expr :: Parser Double
expr =  
    do t <- term
       (<|>) (do { symbol "+"; e <- expr; return $ e + t })
             (do { symbol "-"; e <- expr; return $ e - t })
        <|> return t
    
term :: Parser Double
term = do p <- powe
          (<|>) (do { symbol "*"; t <- term; return $ t * p })
                (do { symbol "/"; t <- term; return $ t / p })
           <|> return p

powe :: Parser Double
powe = do f <- factor
          (<|>) (do { symbol "^"; p <- powe; return $ p ^^ read (takeWhile (/= '.') $ show f)}) 
                (return f)                                 -- Warning: here view 12.2 as 12

factor :: Parser Double
factor = do { symbol ")"; e <- expr; symbol "("; return e }
         <|> realnum

evaluate :: String -> Double
evaluate s = case parser expr s of
                  [(ans, [])] -> ans
                  [(_, s')] -> error ("Unuse input " ++ s')
                  [] -> error "Invalid input"