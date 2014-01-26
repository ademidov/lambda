module Lambda
    ( showExpr
    , readExpr
    ) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (*>), (<*), (<*>))

type VarName = String

data Expr = Var VarName
          | Lam VarName Expr
          | App Expr Expr
          deriving (Eq, Show)

showExpr :: Expr -> String
showExpr (Var v)   = v
showExpr (Lam v e) = "(\\" ++ v ++ "." ++ showExpr e ++ ")"
showExpr (App e1 e2@(App _ _)) = showExpr e1 ++ "(" ++ showExpr e2 ++  ")"
showExpr (App e1 e2)           = showExpr e1 ++ showExpr e2

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
