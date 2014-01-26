module Lambda.Calc
      ( freeVars
      , allVars
      , substitute
      , beta
      , eta
      , nf
      ) where

import Control.Applicative
import Data.List           (union, (\\))

import Lambda.Types

freeVars :: Expr -> [VarName]
freeVars (Var v)     = [v]
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Lam v e)   = freeVars e \\ [v]

allVars :: Expr -> [VarName]
allVars (Var v)     = [v]
allVars (App e1 e2) = allVars e1 `union` allVars e2
allVars (Lam v e)   = [v] `union` allVars e

-- | Examples:
--
-- Variable:
-- >>> showExpr $ substitute (readExpr "u y") "x" (readExpr "x")
-- "uy"
-- >>> showExpr $ substitute (readExpr "u y") "y" (readExpr "x")
-- "x"

-- Application:
-- >>> showExpr $ substitute (readExpr "u z") "x" (readExpr "x y")
-- "uzy"

-- Lambda:
-- >>> showExpr $ substitute (readExpr "u z") "x" (readExpr "\\x.y")
-- "(\\x.y)"
-- >>> showExpr $ substitute (readExpr "u z") "x" (readExpr "\\y.x")
-- "(\\y.uz)"
-- >>> showExpr $ substitute (readExpr "u z") "x" (readExpr "\\y.\\x.xy")
-- "(\\y.(\\x.xy))"
-- >>> showExpr $ substitute (readExpr "y z") "x" (readExpr "\\y.x")
-- "(\\y0.yz)"
substitute :: Expr     -- ^ Expression to be substituted into body
           -> VarName  -- ^ Variable name
           -> Expr     -- ^ Body
           -> Expr     -- ^ Resulting expression
substitute t x (App e1 e2) = App (substitute t x e1) (substitute t x e2)
substitute t x (Var y) | x == y = t
                       | otherwise = Var y
substitute t x (Lam y e)
  | x == y = Lam y e
  | x `notElem` freeVars e || y `notElem` freeVars t = Lam y (substitute t x e)
  | otherwise = Lam newVarName (substitute t x $ substitute newVar y e)
  where newVar     = Var newVarName
        newVarName = nextFreeName (freeVars t `union` freeVars e) y

beta :: Expr -> Maybe Expr
beta (Var _)           = Nothing
beta (Lam x s)         = Lam x <$> beta s
beta (App (Lam x s) t) = Just $ substitute t x s
beta (App t s)         =  App <$> Just t <*> beta s
                      <|> App <$> beta t <*> Just s

nf :: Expr -> Expr
nf t = case beta t of
    Nothing -> t
    Just s  -> nf s

eta :: Expr -> Maybe Expr
eta (Lam x (App t (Var y))) | x == y && x `notElem` freeVars t = Just t
eta _ = Nothing

nextFreeName :: [VarName] -> VarName -> VarName
nextFreeName vars x = new 0
  where
    new :: Int -> VarName
    new n | newVar `elem` vars = new (n + 1)
          | otherwise          = newVar
      where newVar = x ++ show n
