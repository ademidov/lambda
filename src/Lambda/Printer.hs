module Lambda.Printer
    ( showExpr
    ) where

import Lambda.Types

showExpr :: Expr -> String
showExpr (Var v)   = v
showExpr (Lam v e) = "(\\" ++ v ++ "." ++ showExpr e ++ ")"
showExpr (App e1 e2@(App _ _)) = showExpr e1 ++ "(" ++ showExpr e2 ++  ")"
showExpr (App e1 e2)           = showExpr e1 ++ showExpr e2
