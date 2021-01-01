{-# LANGUAGE TupleSections #-}

module Advent10 where

-- see https://crypto.stanford.edu/pbc/notes/zdd/zdd.html

import Data.List (sort, group, tails)
import Control.Arrow (Arrow((&&&)))
import Data.Array

day10 = show . product . map length . group . filter (`elem` [1,3]) . sort . map (uncurry (-)).uncurry zip . (tail &&& id) . sort . (\x -> [0] ++ x ++ [maximum x + 3]) . map read . lines

day10b = show . (\x -> let m = maximum x + 3 in paths 0 m $ edges (\a b -> b <= a + 3) (0 : x ++ [m])) .  sort . map read . lines
  where
  edges p l = q =<< tails l
    where
    q (x:xs) = map (x,) $ takeWhile (p x) xs
    q _ = []
  paths f t l =
    let a = array (f,t) ((t,1) : [(x, sum $ map (a!) (map snd $ filter ((==x) . fst) l)) | x <- [f.. pred t]])
    in a ! f
