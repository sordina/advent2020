{-# LANGUAGE TupleSections #-}
module Advent20 where

import Data.Bool
import Data.List.Split (chunksOf, splitOn)
import Data.Char (isDigit)
import Control.Arrow (second, Arrow((&&&)))
import Data.List (delete, transpose)

day20
    = show . res . head . filter check . map square
    . (\x -> permute (side x) 0 undefined undefined x)
    . map (read . filter isDigit . head &&& tail)
    . splitOn [""]
    . lines

    where
    match     x = and $ zipWith aligned x (tail x)
    aligned a b = and $ zipWith (==) (map last a) (map head b)
    check     x = all (match . map snd) (x ++ transpose (map (map (second transpose)) x))
    square    x = chunksOf (side x) x
    side      x = floor (sqrt (fromIntegral (length x)))

    accept m n p q y
        | n == 0    = True
        | m         = aligned (transpose (snd q)) (transpose (snd y))
        | otherwise = aligned (snd p) (snd y)

    permute _ _ _ _ [] = [[]]
    permute s n p q xs =
        let m = mod n s == 0
        in [y : ys | x <- xs, y <- orient x, accept m n p q y, ys <- permute s (succ n) y (bool q y m) (delete x xs)]

    res    x = product $ map fst [ head $ head x , last $ head x , head $ last x , last $ last x ]
    orient x = flip x ++ flip (second transpose x)
    flip   x = map (`second` x) [ id , reverse , map reverse , reverse . map reverse ]


day20b
    = show . minimum
    . map (sum . map (length . filter (== '#')) . replaceMonster . snd)
    . orient
    . (undefined,)
    . stitch . head . filter check . map square
    . (\x -> permute (side x) 0 undefined undefined x)
    . map (head &&& tail)
    . splitOn [""] . lines

    where

    match     x = and $ zipWith aligned x (tail x)
    aligned a b = and $ zipWith (==) (map last a) (map head b)
    check     x = all (match . map snd) (x ++ transpose (map (map (second transpose)) x))
    square    x = chunksOf (side x) x
    side      x = floor (sqrt (fromIntegral (length x)))

    accept m n p q y
        | n == 0    = True
        | m         = aligned (transpose (snd q)) (transpose (snd y))
        | otherwise = aligned (snd p) (snd y)

    permute _ _ _ _ [] = [[]]
    permute s n p q xs =
        let m = mod n s == 0
        in [y : ys | x <- xs, y <- orient x, accept m n p q y, ys <- permute s (succ n) y (bool q y m) (delete x xs)]

    orient x = flop x ++ flop (second reverse x)
    flop   x = map (`second` x) [ id , rleft, rright, reverse . map reverse ]
    rleft    = reverse . transpose
    rright   = map reverse . transpose
    stitch   = concatMap $ foldr1 (zipWith (++)) . map (init . tail . map (init . tail) . snd)
    monster =
        [ "                  # "
        , "#    ##    ##    ###"
        , " #  #  #  #  #  #   "
        ]

    replaceMonster s = foldr (zipWith2dx replace) s (matches2d monster s)
        where
        replace a b
            | a == '#'  = head "O"
            | otherwise = b

    matches2d p s = [o | y <- [0.. hs-hp], x <- [0.. ws-wp], o <- [offset2d (x,y) p], and $ map and $ zipWith2d f o s]
        where
        hs = length s
        hp = length p
        ws = length (head s)
        wp = length (head p)
        f a b | a == '#' = b == '#'
            | otherwise  = True

    zipWith2d  f   = zipWith (zipWith f) -- faster version for matching
    zipWith2dx f a = zipWith2d f (map (++ spc) a ++ repeat spc)

    spc = cycle " "

    offset2d (x,y) = (replicate y "" ++) . map (take x spc ++)
