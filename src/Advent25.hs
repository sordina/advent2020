module Advent25 where

import Control.Arrow (Arrow((&&&)))

day25
    = show
    . (\((_,k1),(l2,_)) -> iterate (loop k1) 1 !! l2)
    . (head &&& head . tail)
    . map (
        (\x -> head $ filter ((==x) . snd) $ zip [0..] $ iterate (loop 7) 1) . read
    )
    . words

    where
    loop s n = mod (n*s) 20201227