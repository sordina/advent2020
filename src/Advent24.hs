{-# LANGUAGE TupleSections #-}
module Advent24 where

import qualified Data.Map as M

import Data.List (sort, group)
import Control.Arrow ((&&&), Arrow((***)))
import Data.Maybe (fromMaybe, isNothing)

day24
    = show
    . length
    . filter odd
    . map length
    . group
    . sort
    . map ((sum *** sum) . unzip . map coordinate . tokenize)
    . lines

    where
    tokenize []           = []
    tokenize ('e'    :xs) = 0 : tokenize xs
    tokenize ('s':'e':xs) = 1 : tokenize xs
    tokenize ('s':'w':xs) = 2 : tokenize xs
    tokenize ('w'    :xs) = 3 : tokenize xs
    tokenize ('n':'w':xs) = 4 : tokenize xs
    tokenize ('n':'e':xs) = 5 : tokenize xs

    coordinate 0 = (1,0)
    coordinate 1 = (1,-1)
    coordinate 2 = (0,-1)
    coordinate 3 = (-1,0)
    coordinate 4 = (-1,1)
    coordinate 5 = (0,1)


-- Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
-- Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.

day24b
    = show . countBlack . (!!100)
    . iterate (sleep . nextNeighbours)
    . M.fromList
    . map (head &&& even . length)
    . group . sort
    . map ((sum *** sum) . unzip . tokenize)
    . lines

    where
    tokenize []           = []
    tokenize ('e'    :xs) = (1,0)  : tokenize xs
    tokenize ('s':'e':xs) = (1,-1) : tokenize xs
    tokenize ('s':'w':xs) = (0,-1) : tokenize xs
    tokenize ('w'    :xs) = (-1,0) : tokenize xs
    tokenize ('n':'w':xs) = (-1,1) : tokenize xs
    tokenize ('n':'e':xs) = (0,1)  : tokenize xs

    change False 0         = True
    change False n | n > 2 = True
    change True  2         = False
    change x     _         = x

    neighbours (x,y) = map ((+x)***(+y)) centerNeighbours

    centerNeighbours = [(1,0),(1,-1),(0,-1),(-1,0),(-1,1),(0,1)]

    nextNeighbours l = M.union l $ M.fromList $ concatMap notIncluded $ M.toList l
        where
        notIncluded (coords, _color) = map (,True) $ filter (isNothing . flip M.lookup l) (neighbours coords)

    sleep l = M.mapWithKey sleepTile l
        where
        sleepTile coords color = change color (length $ filter not $ map peek (neighbours coords))
        peek x                 = Just False /= M.lookup x l

    countBlack = length . filter not . map snd . M.toList
