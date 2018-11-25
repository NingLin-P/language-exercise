module UseParsec where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
natural     = P.natural lexer
reservedOp  = P.reservedOp lexer
integer     = P.integer lexer
symbol      = P.symbol lexer



expr1 :: Parsec String () Integer
expr1    = term1   `chainl1` addop
term1    = factor1 `chainl1` mulop
factor1  = parens expr1 <|> integer

mulop   =   do{ symbol "*"; return (*)   }
        <|> do{ symbol "/"; return (div) }

addop   =   do{ symbol "+"; return (+) }
        <|> do{ symbol "-"; return (-) }



expr2 :: Parsec String () Integer
expr2    = buildExpressionParser table term2 <?> "expression"

term2    = parens expr2
         <|> natural
         <?> "simple expression"

table   = [  [prefix "-" negate, prefix "+" id ]
           , [postfix "++" (+1)]
           , [binary "*" (*) AssocLeft, binary "/" div AssocLeft ]
           , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
           ]

binary  name fun       = Infix (do{ reservedOp name; return fun })
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })
