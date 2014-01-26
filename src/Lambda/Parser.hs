module Lambda.Parser
    ( readExpr
    ) where

import Control.Applicative ((*>), (<$>), (<*), (<*>))
import Text.Parsec
import Text.Parsec.String

import Lambda.Types

readExpr :: String -> Expr
readExpr = either (error . show) id . parse fullExpr "error"

fullExpr :: Parser Expr
fullExpr = expr <* eof

expr :: Parser Expr
expr = foldl1 App <$> many1 (spaces *> simpleExpr <* spaces)

-- simplicity means unambiguity of associativity rules
simpleExpr :: Parser Expr
simpleExpr = var <|> lam <|> inParens expr
  where inParens = between (char '(') (char ')')

var :: Parser Expr
var = Var <$> varName

lam :: Parser Expr
lam = Lam <$> (lambdaSign *> varName) <*> (char '.' *> expr)
  where lambdaSign = char '\\' <|> char 'λ'

varName :: Parser VarName
varName = (:) <$> lower <*> many digit
