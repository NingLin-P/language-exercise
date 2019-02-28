{-# LANGUAGE FlexibleContexts #-}

module TypeTranspiler where

import Data.List
import Text.Parsec hiding (digit)

alpha = ['a' .. 'z'] ++ ['A' .. 'Z']
digit = ['0' .. '9']

transpile :: String -> Either String String
transpile input = case runParser (typeP <* eof) () "TypeTranspiler" input of
                  Left _ -> Left "Hugh?"
                  Right r -> Right r

ws :: Parsec String () a -> Parsec String () a
ws = between (skipMany $ char ' ') (skipMany $ char ' ')

symbol :: String -> Parsec String () String
symbol s = ws $ string s

name :: Parsec String () String
name = ws $ (try (string "Int" >> return "Integer")) <|>
            (try (string "Unit" >> return "Void"))   <|>
            (oneOf ('_':alpha) >>= \c -> many (oneOf ('_':alpha ++ digit)) >>= return . (c:))

typeParam :: Parsec String () String
typeParam = (string "*" >> return "?") <|>
            try (string "in " >> name >>= return . ("? super "++)) <|>
            try (string "out " >> name >>= return . ("? extends "++)) <|>
            try simpleUserType <|>
            name

typeParams :: Parsec String () String
typeParams = do ts <- sepBy1 typeParam (symbol ",")
                return $ intercalate "," ts

simpleUserType :: Parsec String () String
simpleUserType = name >>= \na -> (<|>) (between (symbol "<") (symbol ">") typeParams >>= \tps -> return (na ++ "<" ++ tps ++ ">"))
                                       (return na)

userType :: Parsec String () String
userType = simpleUserType >>= \sUT -> (<|>) (string "." >> userType >>= return . ((sUT ++ ".") ++))
                                            (return sUT)

functionType :: Parsec String () String
functionType = do para <- between (symbol "(") (symbol ")") (try parameters <|> return [])
                  _ <- symbol "->"
                  t <- typeP
                  return $ "Function" ++ (show $ length para) ++"<"++ concatMap (++",") para ++ t ++">"

typeP :: Parsec String () String
typeP = try functionType <|> try userType <|> name

parameters :: Parsec String () [String]
parameters = typeP >>= \t -> (<|>) (symbol "," >> parameters >>= return . (t:))
                                   (return [t])