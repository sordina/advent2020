module Advent18 where

import Data.Char (isSpace, isDigit)
import Data.Either (rights)
-- TODO: Install parsec
-- import Text.Parsec
-- import Text.Parsec.Expr

day18 = show . sum . map (parse . reverse . filter (not . isSpace)) . lines
    where

    parse :: String -> Int
    parse (x:xs)
        | isDigit x  = op (read [x]) xs
        | x == ')' = let (as,bs) = paren 1 "" xs in op (parse as) bs

    op :: Int -> String -> Int
    op n [] = n
    op n (x:xs)
        | x == '+' = n + parse xs
        | x == '*' = n * parse xs

    paren :: Int -> String -> String -> (String, String)
    paren 1 a (x:xs) | x == '(' = (a,xs)
    paren n a (x:xs) | x == '(' = paren (pred n) (a++"(") xs
    paren n a (x:xs) | x == ')' = paren (succ n) (a++")") xs
    paren n a (x:xs)            = paren n        (a++[x]) xs


day18b = show . sum . rights . map (parse expr "homework" . filter (not . isSpace)) . lines
    where
    parse = undefined
    expr  = undefined
-- let
-- 	expr  = buildExpressionParser table term
-- 	term  = choice [between (string "(") (string ")") expr, nat]
-- 	nat   = read <$> many1 digit
-- 	table = [ [binary "+" (+) AssocLeft] , [binary "*" (*) AssocLeft] ]

-- 	binary :: String -> (Int -> Int -> Int) -> Assoc -> Operator String () Identity Int
-- 	binary name fun assoc = Infix (string name >> return fun) assoc

