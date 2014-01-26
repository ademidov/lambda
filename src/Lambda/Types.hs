module Lambda.Types
    ( VarName
    , Expr (..)
    ) where

type VarName = String

data Expr = Var VarName
          | Lam VarName Expr
          | App Expr Expr
          deriving (Eq, Show)
