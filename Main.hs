module Main where

import Lexer
import Parser

main :: IO ()
main = do
    txt <- getLine
    print (parse $ alexCanTokens txt)