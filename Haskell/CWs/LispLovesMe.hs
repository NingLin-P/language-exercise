module LispLovesMe where

import Text.Parsec

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)
--
assocL f = foldl1 $ match2Args (\i j -> I32 $ f i j)

match2Args f (I32 i) (I32 j) = f i j
match2Args _ _ _ = Err

match2 f [ I32 i, I32 j ] = f i j
match2 _ _ = Err

matchL f [Lst l] = f l
matchL _ _ = Err

matchB f [Boo b] = f b
matchB _ _ = Err

match3B f (Boo b:i:ns) = f b i ns
match3B _ _ = Err

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", assocL (+))
  , ("*", assocL (*))
  , ("-", assocL (-))
  , ("/", assocL div)
  , ("^", match2 (\i j -> I32 $ i ^ j))
  , (">", match2 (\i j -> Boo $ i > j))
  , ("<", match2 (\i j -> Boo $ i < j))
  , ("!", matchB (Boo . not))
  , ("list", Lst)
  , ("size", matchL (I32 . length))
  , ("reverse", matchL (Lst . reverse))
  , ("..", match2 (\i j  -> Lst $ map I32 [i..j]))
  , ("==", match2 (\i j  -> Boo $ i == j))
  , (">=", match2 (\i j  -> Boo $ i >= j))
  , ("<=", match2 (\i j  -> Boo $ i <= j))
  , ("!=", match2 (\i j  -> Boo $ i /= j))
  , ("if", match3B (\b i ns -> if b then i else case ns of
                                                [j] -> j
                                                [] -> Nul
                                                _ -> Err))
  ]

lispPretty :: String -> Maybe String
lispPretty s = case runParser prettyExpr () "Lisp" s of
               Left _ -> Nothing
               Right ast -> Just $ helper ast
    where helper (Lst (n:ns)) = '(' : (helper n) ++ (concatMap ((" "++) . helper) ns) ++ ")"
          helper (I32 i) = show i
          helper (Boo True) = "true"
          helper (Boo False) = "false"
          helper (Sym op) = op
          helper Nul = "null"

prettyExpr :: Parsec String u AST
prettyExpr = try (ws i32) <|> try (ws nul) <|> try (ws boo) <|> try (ws sym) <|> try (ws prettyNod)

prettyNod :: Parsec String u AST
prettyNod = parens $ do items <- many1 prettyExpr
                        return $ Lst items

expr :: Parsec String u AST
expr = try (ws i32) <|> try (ws nul) <|> try (ws boo) <|> try (ws sym) <|> try (ws nod)

ws = between (skipMany $ oneOf " ,\r\n\t") (skipMany $ oneOf " ,\r\n\t")

parens = between (char '(') (char ')')

i32 :: Parsec String u AST
i32 = do n <- many1 digit
         return $ I32 $ read n

nul :: Parsec String u AST
nul = do _ <- string "null" <|> string "()"
         return Nul

boo :: Parsec String u AST
boo = do s <- string "true" <|> string "false"
         return $ Boo (s == "true")

sym :: Parsec String u AST
sym = do c <- noneOf $ " ,\n\t\r()" ++ [ '0' .. '9' ]
         cs <- many $ noneOf " ,\n\t\r()"
         return $ Sym (c:cs)

nod :: Parsec String u AST
nod = parens $ do items <- many1 expr
                  return $ nodEval (head items) (tail items)

nodEval (Sym op) args = case lookup op preludeFunctions of
                        Nothing -> Err
                        Just f -> f args
nodEval _ _ = Err

lispEval :: String -> Maybe AST
lispEval s = case runParser expr () "Lisp" s of
             Left _ -> Nothing
             Right lisp -> Just lisp