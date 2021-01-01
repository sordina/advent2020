module Advent22 where

day22 = undefined
day22b = undefined

-- hoe2 '
-- let
-- f ([],d) = d
-- f (b,[]) = b
-- f (a:b,c:d)
-- 	| a > c     = f (b ++ [a,c], d)
-- 	| otherwise = f (b, d ++ [c,a])

-- r :: String -> Int
-- r = read

-- in
-- sum
-- . zipWith (*) [1..]
-- . reverse
-- . f
-- . (head &&& last)
-- . map (tail . map r)
-- . splitOn [[]]
-- '

-- #!/bin/sh

-- hoe2 -m Debug.Trace '
-- let
-- f p h@([],d) = h
-- f p h@(b,[]) = h
-- f p h@(a:b,c:d)
-- 	| S.member h p = f hp (b ++ [a,c], d)
-- 	| a <= length b && c <= length d =
-- 		case f S.empty (take a b, take c d) of
-- 			(_, []) -> f hp (b ++ [a,c], d)
-- 			_       -> f hp (b, d ++ [c,a])
-- 	| a > c     = f hp (b ++ [a,c], d)
-- 	| otherwise = f hp (b, d ++ [c,a])
-- 	where
-- 	hp = S.insert h p

-- in
-- sum
-- . zipWith (*) [1..]
-- . reverse
-- . uncurry (++)
-- . f S.empty
-- . (head &&& last)
-- . map (tail . map read)
-- . splitOn [[]]
-- '
