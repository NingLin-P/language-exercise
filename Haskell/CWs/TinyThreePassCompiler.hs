module TinyThreePassCompiler where

import Data.List
import Text.Parsec hiding (digit)

satisfyToken :: (Token -> Bool) -> Parsec [Token] u Token
satisfyToken f = tokenPrim show pos testTok
   where testTok t  = if f t then Just t else Nothing
         pos p _ _  = p

tChar c = satisfyToken f
    where f (TChar t) = c == t
          f _ = False

tStr str = satisfyToken f
    where f (TStr s) = str == s
          f _ = False

tString = satisfyToken f
    where f (TStr _) = True
          f _ = False

tInteger = satisfyToken f
    where f (TInt _) = True
          f _ = False

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1

-- pass1
pass1 :: String -> AST
pass1 str = case runParser pass1P () "PASS1" (tokenize str) of
            Left e -> error $ show e
            Right v -> v

pass1P    = do { args <- brackets $ many tString; expr args }
parens    = between (tChar '(') (tChar ')')
brackets  = between (tChar '[') (tChar ']')
pInt      = do { (TInt n) <- tInteger; return $ Imm n }
pVar args = foldr1 (<|>) (zipWith toP args [0..])
    where toP (TStr s) n = do { tStr s; return $ Arg n }

expr :: [Token] -> Parsec [Token] u AST
expr   args  = term   args  `chainl1` addop
term   args  = factor args  `chainl1` mulop
factor args  = parens (expr args) <|> pInt <|> pVar args
mulop   =   do{ tChar '*'; return Mul }
        <|> do{ tChar '/'; return Div }
addop   =   do{ tChar '+'; return Add }
        <|> do{ tChar '-'; return Sub }

-- pass1
pass2 :: AST -> AST
pass2 (Add a b) = pass2 a `add` pass2 b
pass2 (Sub a b) = pass2 a `sub` pass2 b
pass2 (Mul a b) = pass2 a `mul` pass2 b
pass2 (Div a b) = pass2 a `dIV` pass2 b
pass2 ast       = ast

add (Imm a) (Imm b) = Imm $ a + b
add a b = Add a b
sub (Imm a) (Imm b) = Imm $ a - b
sub a b = Sub a b
mul (Imm a) (Imm b) = Imm $ a * b
mul a b = Mul a b
dIV (Imm a) (Imm b) = Imm $ a `div` b
dIV a b = Div a b

-- pass3
pass3 :: AST -> [String]
pass3 (Imm   n) = ["IM " ++ show n]
pass3 (Arg   n) = ["AR " ++ show n]
pass3 (Add a b) = step a b ++ ["AD"]
pass3 (Sub a b) = step a b ++ ["SU"]
pass3 (Mul a b) = step a b ++ ["MU"]
pass3 (Div a b) = step a b ++ ["DI"]

step :: AST -> AST -> [String]
step i@(Imm _) a@(Arg _) = pass3 i ++ ["SW"] ++ pass3 a
step a@(Arg _) i@(Imm _) = pass3 a ++ ["SW"] ++ pass3 i
step a b = pass3 a ++ ["PU"] ++ pass3 b ++ ["SW","PO"]

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0,_,_) = r0

