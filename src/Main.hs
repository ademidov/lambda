module Main where

import Data.List (intercalate)

import Lambda.Parser
import Lambda.Printer
import Lambda.Calc

main :: IO ()
main = do
    expr <- fmap readExpr getLine
    print expr
    putStrLn $ showExpr expr
    putStrLn $ "Free vars: " ++ intercalate ", " (freeVars expr)
    putStrLn $ "All vars: "  ++ intercalate ", " (allVars expr)
