
module Advent04 where

import Data.List.Split
import Data.Maybe (isJust, fromJust)
import Control.Arrow (Arrow(second))
import Data.Char (isDigit)

day4
    = show . length
    . filter (flip all (words "byr iyr eyr hgt hcl ecl pid") . flip elem) . map (map (take 3) . words) . splitOn "\n\n"

day4b
    = show .length
    . filter (all (\(a,b)->fromJust (lookup a (("cid", const True):f)) b)) . filter g . map (map (second tail . splitAt 3) . words) . splitOn "\n\n"
    where
    g x = all ((isJust . flip lookup x) . fst) f
    f =
        [ ("byr", (\x->x>1919&&x<2003).read)
        , ("iyr", (\x->x>2009&&x<2021).read)
        , ("eyr", (\x->x>2019&&x<2031).read)
        , ("hgt", \x-> (\d h->(d=="cm"&&h>149&&h<194)||(d=="in"&&h>58&&h<77))(dropWhile isDigit x) (read$ takeWhile isDigit x))
        , ("hcl", \x->length x==7 && take 1 x=="#" && all (`elem` "0123456789abcdef") (drop 1 x))
        , ("ecl", flip elem (words "amb blu brn gry grn hzl oth"))
        , ("pid", \x-> length x == 9 && all isDigit x)
        ]