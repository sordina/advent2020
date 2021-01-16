{-# LANGUAGE TypeApplications #-}

module Advent16 where

import Control.Arrow (second, Arrow((&&&)))
import Data.List.Split (splitOn)
import Data.List (isPrefixOf)

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f = not . any f

day16
    = show . sum
    . (\(r,l) -> filter (rules r) l)
    . (
        map (map ((head &&& last) . map (read @Int)) . filter ((==2) . length) . map (splitOn "-") . words) . head
        &&&
        concatMap (read @[Int] . (\x -> "["++x++"]")) . tail . last
    )
    . splitOn [""]
    . lines
    where
    mkRule r n = any (\(a,b) -> n >= a && n <= b) r
    rules r x = none ($ x) (map mkRule r)

day16b
    = show . product
    . (\(m,l) -> map (head m!!) (concatMap snd l))
    . second (filter (isPrefixOf "departure " . fst) . fixf r2)
    . (\(m, r,l) -> (m, map (f [0..pred (length r)] l) r))
    . (\(r,(l,m)) -> (m, r, filter (none (rules (map snd r))) l))
    . (
        map
            (
                takeWhile (not . flip elem ":")
                &&&
                map ((head &&& last) . map (read @Int))
                . filter ((==2) . length) . map (splitOn "-") . words
            ) . head
        &&&
        q . last &&& q . (!!1)
    )
    . splitOn [""] . lines

    where
    reduce l    = map (\m -> filter (\n-> sing m || notElem n (concat (filter sing l))) m) l
    sing        = (==1) . length
    fixf f      = fst . until (uncurry (==)) ((f &&& id) . fst) . (f&&&id)
    r2 l        = let (a,b) = unzip l in zip a $ reduce b
    mkRule r n  = any (\(a,b) -> n >= a && n <= b) r
    rules r x   = none ($ x) (map mkRule r)
    f a l (n,r) = (n, filter (\i -> all (mkRule r . (!!i)) l) a)
    q           = map (read @[Int] . (\x -> "["++x++"]")) . tail
