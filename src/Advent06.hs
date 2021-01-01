
module Advent06 where

import Data.List (group, sort, nub)
import Data.List.Split (splitOn)

day6 = show . sum . map (length . nub . sort . concat) . splitOn [""] . lines

day6b = show . sum . map f . splitOn [""] . lines
    where
    f x = (length . filter (length x==). map length . group . sort . concat) x
