module SimpleInteractiveInterpreter where

import Text.Parsec
import Data.Maybe


type Interpreter = [(String, Ident)]
type Result = Maybe Double

data Ident =
    Val Double |
    Fn {
        agrs :: [String],
        fnExpr :: String
    } deriving (Show)

newInterpreter :: Interpreter
newInterpreter = []

repl :: Interpreter -> String -> IO ()
repl sta str = case input str sta of
               Left e -> putStrLn e
               Right res -> print res

input :: String -> Interpreter -> Either String (Result, Interpreter)
input str sta = case runParser ((try (expr sta) <|> emp) <* eof) () "Interactive" str of
                Left e -> Left $ show e
                Right v -> Right v

expr :: Interpreter -> Parsec String () (Result, Interpreter)
expr sta   = ws $ term sta  `chainl1` addop
term sta   = ws $ factor sta `chainl1` mulop
factor sta = ws (parens (expr sta) <|> num sta <|> try (fn sta) <|> try (assign sta) <|> val sta)

mulop   =       do{ ws $ char '*'; return (\(Just a, s1) (Just b, s2) -> (Just (a*b), mergSta s1 s2)) }
            <|> do{ ws $ char '/'; return (\(Just a, s1) (Just b, s2) -> (Just (a/b), mergSta s1 s2)) }
            <|> do{ ws $ char '%'; return myMod }

addop   =       do{ ws $ char '+'; return (\(Just a, s1) (Just b, s2) -> (Just (a+b), mergSta s1 s2)) }
            <|> do{ ws $ char '-'; return (\(Just a, s1) (Just b, s2) -> (Just (a-b), mergSta s1 s2)) }

-- poor to apply mod on double
myMod (Just a, s1) (Just b, s2) = (Just $ int2Double (double2Int a `mod` double2Int b), mergSta s1 s2)

double2Int :: Double -> Int
double2Int b = if int2Double int == b 
               then int
               else error "Can not apply % to flaot point number"
    where int = read (takeWhile (/= '.') $ show b) :: Int

int2Double :: Int -> Double
int2Double i = (read $ show i) :: Double

mergSta :: Interpreter -> Interpreter -> Interpreter
mergSta [] s = s
mergSta s [] = s
mergSta (s@(name, _):s1) s2 = case lookup name s2 of
                              (Just _) -> mergSta s1 s2
                              Nothing -> mergSta s1 (s:s2)

confli :: String -> Bool -> Interpreter -> Bool
confli name isVal sta = case lookup name sta of
                        Nothing -> False
                        (Just (Val _)) -> not isVal
                        _ -> isVal

parens :: Parsec String u r -> Parsec String u r
parens = between (char '(') (char ')')

emp :: Parsec String () (Result, Interpreter)
emp = do { skipMany $ char ' '; return (Nothing, [])}

ws :: Parsec String () a -> Parsec String () a
ws = between (skipMany $ char ' ') (skipMany $ char ' ')

num :: Interpreter -> Parsec String () (Result, Interpreter)
num sta = do n <- many1 digit
             (<|>) (do { char '.'; f <- many1 digit; return (Just $ read (n++"."++f), sta) })
                   (return (Just $ read n, sta))

ident :: Parsec String () String
ident = ws $ do c <- letter
                cs <- many $ alphaNum <|> char '_'
                return (c:cs)

val :: Interpreter -> Parsec String () (Result, Interpreter)
val sta = do name <- ident
             case lookup name sta of
                  (Just (Val v)) -> return (Just v, sta)
                  (Just (Fn args funExp)) -> do realArg <- count (length args) $ expr sta
                                                case input funExp (zip args (map (Val . fromJust . fst) realArg)) of
                                                 Left e -> unexpected e
                                                 Right (res, _) -> return (res, sta)
                  _ -> unexpected $ "not have such variable: " ++ name

assign :: Interpreter -> Parsec String () (Result, Interpreter)
assign sta = do name <- ident
                char '='
                (Just v, sta') <- expr sta
                if confli name True sta' 
                then unexpected $ "Variable name conflict: " ++ name
                else return (Just v, (name, Val v) : sta')

fn :: Interpreter -> Parsec String () (Result, Interpreter)
fn sta = do string "fn"
            name <- ident
            args <- many ident
            ws $ string "=>"
            funExp <- many1 anyChar
            if confli name False sta 
            then unexpected $ "Function name conflict: " ++ name
            -- a poor way to check invalid variable within function body
            else case input funExp (zip args (map Val [1.0 ..])) of
                 Right (_,_) -> return (Nothing, (name, Fn args funExp):sta)
                 Left e -> unexpected e