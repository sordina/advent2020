{-# LANGUAGE TupleSections #-}

module Advent21 where

import qualified Data.Map as M

import Control.Arrow ((&&&))
import Data.List (nub, sort, intercalate, intersect, (\\))
import Data.Bifunctor (Bifunctor(bimap))

fixf :: Eq a => (a -> a) -> a -> a
fixf f = fst . head . dropWhile (uncurry (/=)) . uncurry zip . (id &&& tail) . iterate f

day21
    = show . length . concatMap fst . fixf reduce
    . map (words . takeWhile (/= '(') &&& tail . words . filter (/= ',') . init . dropWhile (/= '('))
    . lines

    where
    reduce l = map (bimap (\\ concatMap snd s) (\\ map fst s)) l
        where
        s = filter ((==1).length.snd)
            $ M.toList
            $ foldr (M.unionWith intersect) M.empty
            $ concatMap (\(i,a) -> [M.singleton l i | l<-a]) l

day21b
    = intercalate "," . concatMap snd . sort . fst . fixf reduce . ([],)
    . map (words . takeWhile (/= '(') &&& tail . words . filter (/= ',') . init . dropWhile (/= '('))
    . lines

    where
    reduce (o,l) = (nub (o ++ s), map (bimap (\\ concatMap snd s) (\\ map fst s)) l)
        where
        s = filter ((==1).length.snd)
            $ M.toList
            $ foldr (M.unionWith intersect) M.empty
            $ concatMap (\(i,a) -> [M.singleton l i | l<-a]) l