module Advent15 where

day15 = undefined
day15b = undefined

-- hoe2 -m 'Control.Lens.Operators Control.Monad.State' '
-- let

-- 	f :: Int -> State (Int, M.Map Int Int) (Int, M.Map Int Int)
-- 	f n = do
-- 		i <- _1 <<+= 1
-- 		m <- _2 <<%= M.insert n i
-- 		return (i,m)

-- 	g n = do
-- 		(i,m) <- f n
-- 		if i < 2020
-- 		then maybe (g 0) (g . (i-)) (M.lookup n m)
-- 		else return n

-- 	s = [16,12,1,0,15,7,11]

-- in

-- (1, M.empty)
-- &~ do
-- 		mapM_ f (init s)
-- 		n <- _2 <%= id
-- 		o <- g (last s)
-- 		_1 .= o
-- & view _1
-- '

-- #!/bin/sh

-- hoe2 -m 'Control.Lens.Operators Control.Monad.State' '
-- let

-- 	f :: Int -> State (Int, M.Map Int Int) (Int, M.Map Int Int)
-- 	f n = do
-- 		i <- _1 <<+= 1
-- 		m <- _2 <<%= M.insert n i
-- 		return (i,m)

-- 	g :: Int -> State (Int, M.Map Int Int) Int
-- 	g n = do
-- 		(i,m) <- f n
-- 		if i < 30000000
-- 		then
-- 			case M.lookup n m of
-- 				Just j  -> g (i-j)
-- 				Nothing -> g 0
-- 		else return n

-- 	s = [16,12,1,0,15,7,11]

-- in

-- (1, M.empty)
-- &~ do
-- 		mapM_ f (init s)
-- 		n <- _2 <%= id
-- 		o <- g (last s)
-- 		_1 .= o
-- & view _1
-- '
