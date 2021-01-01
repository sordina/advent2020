{-# LANGUAGE TupleSections #-}

module Advent13 where

import Control.Arrow (second, (***), Arrow((&&&)))
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Maybe    (fromJust)

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.Chinese

day13
  = show
  . (\(a,(b,c)) -> b * mod c a)
  . second (minimumBy (compare `on` snd))
  . (\(x,y) -> (x,) $ map (\z -> (z,) $ head $ dropWhile (<x) $ iterate (+z) 0) y)
  . second (map read . words)
  . (read.(!!0) &&& concatMap (\x-> if elem x "x," then " " else [x]).(!!1))
  . lines

-- https://www.reddit.com/r/learnprogramming/comments/7bcw31/least_common_multiple_with_an_offset/
-- https://math.stackexchange.com/questions/2218763/how-to-find-lcm-of-two-numbers-when-one-starts-with-an-offset
-- https://en.wikipedia.org/wiki/Least_common_multiple#Calculation
-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
-- https://en.wikipedia.org/wiki/Bézout%27s_identity
-- https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Bézout's_identity_and_extended_GCD_algorithm

day13b
  = show
  . uncurry (-)
  . (read.init.last &&& read.tail.head) . words . show
  . foldl1 (\a b -> fromJust $ chineseSomeMod a b)
  . map (uncurry modulo)
  . map (id *** read)
  . filter ((/="x").snd) . zip [0..] .  words
  . concatMap (\x-> if elem x "," then " " else [x])
  . (!!1) .  lines
