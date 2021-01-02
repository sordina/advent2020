module Advent22 where

import Control.Arrow (Arrow((&&&)))
import Data.List.Split (splitOn)
import qualified Data.Set as S

day22
    = show . sum . zipWith (*) [1..] . reverse . f
    . (head &&& last) . map (tail . map read) . splitOn [[]] . lines

    where
    f ([],d) = d
    f (b,[]) = b
    f (a:b,c:d)
        | a > c     = f (b ++ [a,c], d)
        | otherwise = f (b, d ++ [c,a])

day22b
    = show . sum . zipWith (*) [1..] . reverse . uncurry (++) . f S.empty
    . (head &&& last) . map (tail . map read) . splitOn [[]] . lines

    where
    f _ h@([],_) = h
    f _ h@(_,[]) = h
    f p h@(a:b,c:d)
      | S.member h p = f hp (b ++ [a,c], d)
      | a <= length b && c <= length d =
        case f S.empty (take a b, take c d) of
          (_, []) -> f hp (b ++ [a,c], d)
          _       -> f hp (b, d ++ [c,a])
      | a > c     = f hp (b ++ [a,c], d)
      | otherwise = f hp (b, d ++ [c,a])
      where
      hp = S.insert h p
