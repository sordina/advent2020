module Advent21 where

day21 = undefined
day21b = undefined

-- hoe2 '
-- let
-- isnt x = not . flip elem x
-- fixf f = fst . head . dropWhile (uncurry (/=)) . uncurry zip . (id &&& tail) . iterate f
-- reduce l = map (second (\\ map fst s) . first (\\ concatMap snd s)) l
-- 	where
-- 	s = filter ((==1).length.snd)
-- 		$ M.toList
-- 		$ foldr (M.unionWith intersect) M.empty
-- 		$ concatMap (\(i,a) -> [M.singleton l i | l<-a])
-- 		$ l
-- in
-- length
-- .
-- concatMap fst
-- .
-- fixf reduce
-- .
-- map (words . takeWhile (isnt "(") &&& tail . words . filter (isnt ",") . init . dropWhile (isnt "("))
-- .
-- lines
-- '

-- #!/bin/sh

-- hoe2 '
-- let
-- isnt x = not . flip elem x
-- fixf f = fst . head . dropWhile (uncurry (/=)) . uncurry zip . (id &&& tail) . iterate f
-- reduce (o,l) = (nub (o ++ s), map (second (\\ map fst s) . first (\\ concatMap snd s)) l)
-- 	where
-- 	s = filter ((==1).length.snd)
-- 		$ M.toList
-- 		$ foldr (M.unionWith intersect) M.empty
-- 		$ concatMap (\(i,a) -> [M.singleton l i | l<-a])
-- 		$ l
-- in
-- intercalate ","
-- .
-- concatMap snd
-- .
-- sort
-- .
-- fst
-- .
-- fixf reduce
-- .
-- ([],)
-- .
-- map (words . takeWhile (isnt "(") &&& tail . words . filter (isnt ",") . init . dropWhile (isnt "("))
-- .
-- lines
-- '
