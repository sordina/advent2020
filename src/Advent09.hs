module Advent09 where

import Data.List (tails)
import Control.Arrow (Arrow((&&&)))

day9 = show . last . head . filter (\x -> null [1 | a <- init x, b <- init x, last x == a + b]) . map (map read . take 26) . tails . lines

day9b = show . uncurry (+) . (minimum &&& maximum) . head . filter ((==675280050) . sum) . (\x -> [2..] >>= (\n -> map (take n . map read) x)) . tails . lines

