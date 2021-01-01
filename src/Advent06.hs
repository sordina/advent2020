
module Advent06 where

import Data.List (sort, nub)
import Data.List.Split (splitOn)

day6 = show . sum . map (length . nub . sort . concat) . splitOn [""] . lines

day6b = undefined
