module Main where

import Lambda.Parser
import Lambda.Printer

main :: IO ()
main = do
    expr <- fmap readExpr getLine
    print expr
    putStrLn $ showExpr expr
