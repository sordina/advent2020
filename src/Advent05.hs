
module Advent05 where

import Data.Bool (bool)

day5
    = show . maximum
    . map (sum . zipWith (*) (map(2^)[0..]) . map (bool 0 1.flip elem"BR") .reverse)
    . lines

day5b
    = show
    . (\x->filter(not.flip elem x)[minimum x..maximum x])
    . map (sum . zipWith (*) (map (2^) [0..]) . map (bool 0 1.flip elem"BR") . reverse)
    . lines

