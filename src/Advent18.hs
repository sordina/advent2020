module Advent18 where

day18 = undefined
day18b = undefined

-- hoe2 '
-- let
-- parse :: String -> Int
-- parse (x:xs)
-- 	| isDigit x  = op (read [x]) xs
-- 	| elem x ")" = let (as,bs) = paren 1 "" xs in op (parse as) bs

-- op :: Int -> String -> Int
-- op n [] = n
-- op n (x:xs)
-- 	| elem x "+" = n + parse xs
-- 	| elem x "*" = n * parse xs

-- paren :: Int -> String -> String -> (String, String)
-- paren 1 a (x:xs) | elem x "(" = (a,xs)
-- paren n a (x:xs) | elem x "(" = paren (pred n) (a++"(") xs
-- paren n a (x:xs) | elem x ")" = paren (succ n) (a++")") xs
-- paren n a (x:xs)              = paren n        (a++[x]) xs

-- in
-- sum
-- .
-- map (parse . reverse . filter (not . isSpace))
-- '


-- #!/bin/sh

-- hoe2 -m 'Text.Parsec Text.Parsec.Expr' '
-- let
-- 	expr  = buildExpressionParser table term
-- 	term  = choice [between (string "(") (string ")") expr, nat]
-- 	nat   = read <$> many1 digit
-- 	table = [ [binary "+" (+) AssocLeft] , [binary "*" (*) AssocLeft] ]

-- 	binary :: String -> (Int -> Int -> Int) -> Assoc -> Operator String () Identity Int
-- 	binary name fun assoc = Infix (string name >> return fun) assoc
-- in
-- sum .  rights .  map (parse expr "homework" . filter (not . isSpace))
-- '

