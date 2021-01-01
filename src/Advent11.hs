module Advent11 where

day11 = undefined
day11b = undefined

-- hoe "
-- let
-- 	rules ('L', 0)          = '#'
-- 	rules ('#', n) | n >= 4 = 'L'
-- 	rules (x  , _)          = x

-- 	p m x = filter (\x -> x >= 0 && x < m) [pred x .. succ x]

-- 	index (w,h) = [[ ((x,y), [(i,j) | j <- p h y, i <- p w x, (i,j)/=(x,y)]) | x<-[0.. pred w]] | y<-[0.. pred h]]

-- 	foo l (x,y) = l !! y !! x

-- 	bar = length . filter (== '#')
-- 	baz
-- 		= unlines
-- 		.
-- 		(\(is,l) -> map (map (rules . (foo l *** bar . map (foo l)))) is)
-- 		.
-- 		(index *** id)
-- 		.
-- 		((length . head &&& length) &&& id)
-- 		.
-- 		lines
-- in
-- length . filter (=='#') . snd . head . dropWhile (uncurry (/=)) . uncurry zip . (id &&& tail) . iterate baz
-- " # < advent11.input.small


-- # If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
-- # If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
-- # Otherwise, the seat's state does not change.


-- #!/bin/sh


-- hoe -m 'Data.Array' "
-- let
-- 	rules ('L', 0)          = '#'
-- 	rules ('#', n) | n >= 5 = 'L'
-- 	rules (x  , _)          = x

-- 	n = tail [(f***g)|l<-[[id,pred,succ]], f<-l, g<-l]

-- 	ix b x = x < b && x >= 0

-- 	ib h w (y,x) = ix h y && ix w x

-- 	q l y x h w = n >>= take 1 . dropWhile (== '.') . map (l!) . takeWhile (ib h w) . tail . flip iterate (y,x)

-- 	index l (h,w) = [[ ((y,x), q l y x h w) | x<-[0.. pred w]] | y<-[0.. pred h]]

-- 	bar = length . filter (== '#')

-- 	baz
-- 		= unlines
-- 		.  (\(is,l) -> map (map (rules . ((l!) *** bar))) is)
-- 		.  (\(b,l) -> (index l b, l))
-- 		.  (\(b@(h,w),l) -> (b, listArray ((0,0),(pred h, pred w)) (concat l)))
-- 		.  ((length &&& length . head) &&& id)
-- 		.  lines
-- in
-- sum . map (bar . snd) . take 1 . dropWhile (uncurry (/=)) . uncurry zip . (id &&& tail) . iterate baz
-- "

-- # Also, people seem to be more tolerant than you expected: it now takes five or
-- # more visible occupied seats for an occupied seat to become empty (rather than
-- # four or more from the previous rules). The other rules still apply: empty
-- # seats that see no occupied seats become occupied, seats matching no rule
-- # don't change, and floor never changes.

-- # Otherwise, the seat's state does not change.

-- # As soon as people start to arrive, you realize your mistake. People don't just
-- # care about adjacent seats - they care about the first seat they can see in each
-- # of those eight directions!



