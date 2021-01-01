module Advent12 where

day12 = undefined
day12b = undefined

-- hoe2 -m Control.Monad.State "
-- let
-- 	x :: (Char,Int) -> State (Int,Int,Char) ()
-- 	x = \case
-- 		('N',n) -> _2 += n
-- 		('S',n) -> x ('N',negate n)
-- 		('E',n) -> _1 += n
-- 		('W',n) -> x ('E',negate n)
-- 		('L',90)-> _3 %= fromJust.flip lookup [('E','N'),('S','E'),('W','S'),('N','W')]
-- 		('L',n) -> x ('L',90) >> x ('L',n-90)
-- 		('R',n) -> x ('L',360-n)
-- 		('F',n) -> _3<%=id >>= fromJust . flip lookup [('E',_1+=n),('S',_2-=n),('W',_1-=n),('N',_2+=n)]
-- in
-- 	(\(a,b,_)->abs a+abs b)
-- 	. ((0,0,'E')&~)
-- 	. mapM_ x
-- 	. map (\(l:n)->(l,read n))
-- 	. words
-- "

-- #!/bin/sh

-- hoe2 -m Control.Monad.State "
-- let
-- 	x :: (Char,Int) -> State (Int,Int,(Int,Int)) ()
-- 	x = \case
-- 		('N',n) -> _3._2 += n
-- 		('S',n) -> x ('N',negate n)
-- 		('E',n) -> _3._1 += n
-- 		('W',n) -> x ('E',negate n)
-- 		('L',90)-> _3 %= \(x,y) -> (negate y, x)
-- 		('L',n) -> x ('L',90) >> x ('L',n-90)
-- 		('R',n) -> x ('L',360-n)
-- 		('F',n) -> replicateM_ n $ modify (\(a,b,c@(i,j)) -> (a+i,b+j,c))
-- in
-- 	(\(a,b,_)->abs a+abs b)
-- 	. ((0,0,(10,1))&~)
-- 	. mapM_ x
-- 	. map (\(l:n)->(l,read n))
-- 	. words
-- "

-- # Action N means to move the waypoint north by the given value.
-- # Action S means to move the waypoint south by the given value.
-- # Action E means to move the waypoint east by the given value.
-- # Action W means to move the waypoint west by the given value.
-- # Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
-- # Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
-- # Action F means to move forward to the waypoint a number of times equal to the given value.

-- # F10 moves the ship to the waypoint 10 times (a total of 100 units east and 10 units north), leaving the ship at east 100, north 10. The waypoint stays 10 units east and 1 unit north of the ship.
-- # N3 moves the waypoint 3 units north to 10 units east and 4 units north of the ship. The ship remains at east 100, north 10.
-- # F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28 units north), leaving the ship at east 170, north 38. The waypoint stays 10 units east and 4 units north of the ship.
-- # R90 rotates the waypoint around the ship clockwise 90 degrees, moving it to 4 units east and 10 units south of the ship. The ship remains at east 170, north 38.
-- # F11 moves the ship to the waypoint 11 times (a total of 44 units east and 110 units south), leaving the ship at east 214, south 72. The waypoint stays 4 units east and 10 units south of the ship.
