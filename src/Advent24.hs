module Advent24 where

day24 = undefined
day24b = undefined

-- hoe2 -m Debug.Trace "
-- let

-- tokenize []           = []
-- tokenize ('e'    :xs) = 0 : tokenize xs
-- tokenize ('s':'e':xs) = 1 : tokenize xs
-- tokenize ('s':'w':xs) = 2 : tokenize xs
-- tokenize ('w'    :xs) = 3 : tokenize xs
-- tokenize ('n':'w':xs) = 4 : tokenize xs
-- tokenize ('n':'e':xs) = 5 : tokenize xs

-- coordinate 0 = (1,0)
-- coordinate 1 = (1,-1)
-- coordinate 2 = (0,-1)
-- coordinate 3 = (-1,0)
-- coordinate 4 = (-1,1)
-- coordinate 5 = (0,1)

-- in
-- length
-- .
-- filter odd
-- .
-- map length
-- .
-- group
-- .
-- sort
-- .
-- map ((sum *** sum) . unzip . map coordinate . tokenize)
-- "

-- #!/bin/sh

-- hoe2 -m Debug.Trace "
-- let

-- tokenize []           = []
-- tokenize ('e'    :xs) = (1,0) : tokenize xs
-- tokenize ('s':'e':xs) = (1,-1) : tokenize xs
-- tokenize ('s':'w':xs) = (0,-1) : tokenize xs
-- tokenize ('w'    :xs) = (-1,0) : tokenize xs
-- tokenize ('n':'w':xs) = (-1,1) : tokenize xs
-- tokenize ('n':'e':xs) = (0,1) : tokenize xs

-- change False 0         = True
-- change False n | n > 2 = True
-- change True  2         = False
-- change x     n         = x

-- {-
-- 	Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
-- 	Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
-- -}

-- neighbours tile@(x,y) = map ((+x)***(+y)) centerNeighbours

-- centerNeighbours = [(1,0),(1,-1),(0,-1),(-1,0),(-1,1),(0,1)]

-- nextNeighbours l = M.union l $ M.fromList $ concatMap notIncluded $ M.toList l
-- 	where
-- 	notIncluded (coords,color) = map (,True) $ filter (isNothing . flip M.lookup l) (neighbours coords)

-- sleep l = M.mapWithKey sleepTile l
-- 	where
-- 	sleepTile coords color = change color (length $ filter not $ map peek (neighbours coords))
-- 	peek x                 = fromMaybe True $ M.lookup x l

-- countBlack = length . filter not . map snd . M.toList

-- in
-- 	countBlack
-- 	. (!!100)
-- 	. iterate (sleep . nextNeighbours)
-- 	. M.fromList
-- 	. map (head &&& even . length)
-- 	. group
-- 	. sort
-- 	. map ((sum *** sum) . unzip . tokenize)

-- "
