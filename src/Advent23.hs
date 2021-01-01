module Advent23 where

day23 = undefined
day23b = undefined

-- hoe2 -m Debug.Trace '
-- let

-- d n a b l h
-- 	| elem n a  = d (pred n) a b l h
-- 	| n < l     = d h a b l h
-- 	| otherwise = n

-- rl n c = take l $ drop n (cycle c)     where l = length c

-- move c@(x:xs) = rl 1 $ e ++ a ++ f
-- 	where
-- 	(a,b)  = splitAt 3 xs
-- 	ys     = x:b
-- 	n      = d (pred x) a ys (minimum c) (maximum c)
-- 	(e,f)  = splitAt (succ $ fromJust $ elemIndex n ys) ys

-- main
-- 	= (++"\n")
-- 	. (!! 100)
-- 	. map
-- 	( concatMap show
-- 	. (\l -> take (pred $ length l) $ tail $ dropWhile (/= 1) $ cycle l)
-- 	)
-- 	. iterate move
-- 	. map ((read :: String->Int) . return)
-- 	. head
-- 	. words

-- in
-- main
-- '

-- #!/bin/sh

-- # Note: Too slow! See advent23bvec.hoe for timely solution.

-- hoe2 -m 'Debug.Trace Data.Sequence' '
-- let

-- d :: Int -> Seq Int -> Seq Int -> Int -> Int -> Int
-- d n a b l h
-- 	| elem n a  = d (pred n) a b l h
-- 	| n < l     = d h a b l h
-- 	| otherwise = n

-- rl (x :<| xs) = xs :|> x

-- move l h c@(x :<| xs) = rl $ e >< a >< f
-- 	where
-- 	(a,b)  = Data.Sequence.splitAt 3 xs
-- 	ys     = x :<| b
-- 	n      = d (pred x) a ys l h
-- 	(e,f)  = Data.Sequence.splitAt (succ $ fromJust $ elemIndexL n ys) ys

-- look l@(a :<| b :<| _) = product $ Data.Sequence.take 2 $ Data.Sequence.drop (succ i) (l :|> a :|> b)
-- 	where
-- 	i = fromJust $ elemIndexL 1 l

-- main
-- 	= look
-- 	. (!! 10000)
-- 	. (\x -> iterate (move (minimum x) (maximum x)) x)
-- 	. fromList
-- 	. (\l -> l ++ [succ (maximum l) .. 10000])
-- 	. map ((read :: String->Int) . return)
-- 	. head
-- 	. words

-- in
-- main
-- '
