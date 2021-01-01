module Advent09 where

day9 = undefined
day9b = undefined

-- hoe 'last.head.filter (\x -> null [1 | a <- init x, b <- init x, last x == a + b]).map (map read . take 26).tails.lines'

-- #!/bin/sh
-- hoe 'uncurry (+).(minimum &&& maximum).head.filter ((==675280050) . sum).(\x -> [2..] >>= (\n -> map (take n . map (read :: String -> Int)) x)).tails.lines'

