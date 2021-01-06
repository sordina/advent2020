
module Advent03 where

-- "From your starting position at the top-left, check the position that is
--  right 3 and down 1. Then, check the position that is right 3 and down 1 from
--  there, and so on until you go past the bottom of the map."

day3 = length . filter id . f . map cycle . lines
    where
    f m = zipWith (\x y -> m!!y!!x == '#') [0,3..] [0..length m-1] 

-- What do you get if you multiply together the number of trees encountered on each of the listed slopes?
-- 
-- * Right 1, down 1.
-- * Right 3, down 1. (This is the slope you already checked.)
-- * Right 5, down 1.
-- * Right 7, down 1.
-- * Right 1, down 2.

day3b
    = product . f g . map cycle . lines
    where
    f z m = [sum[1|True<-zipWith(\x y->m!!y!!x=='#')i(takeWhile(<length m)j)]|(i,j)<-[z 1,z 3,z 5,z 7,([0..],[0,2..])]]
    g z = ([0,z..],[0..])

-- >>> day3 testInput
-- 7

-- "In this example, traversing the map using this slope would cause you to encounter 7 trees."

-- >>> day3b testInput
-- 336

-- "In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s)
--  respectively; multiplied together, these produce the answer 336."

testInput = unlines
    [ "..##......."
    , "#...#...#.."
    , ".#....#..#."
    , "..#.#...#.#"
    , ".#...##..#."
    , "..#.##....."
    , ".#.#.#....#"
    , ".#........#"
    , "#.##...#..."
    , "#...##....#"
    , ".#..#...#.#"
    ]
