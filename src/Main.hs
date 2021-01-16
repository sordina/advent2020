module Main where

import Data.Maybe
import System.Environment ( getArgs )
import Data.List (intercalate)

import qualified Advent01
import qualified Advent02
import qualified Advent03
import qualified Advent04
import qualified Advent05
import qualified Advent06
import qualified Advent07
import qualified Advent08
import qualified Advent09
import qualified Advent10
import qualified Advent11
import qualified Advent12
import qualified Advent13
import qualified Advent14
import qualified Advent15
import qualified Advent16
import qualified Advent17
import qualified Advent18
import qualified Advent19
import qualified Advent19ReadP
import qualified Advent20
import qualified Advent21
import qualified Advent22
import qualified Advent23
import qualified Advent24
import qualified Advent25
import Control.Arrow (Arrow((&&&)))

interactShow :: Show a => (String -> a) -> IO ()
interactShow f = interact (show . f)

days :: [(String, IO ())]
days =
    [ ("1",   interactShow Advent01.day1)
    , ("1b",  interactShow Advent01.day1b)
    , ("2",   interactShow Advent02.day2)
    , ("2b",  interactShow Advent02.day2b)
    , ("3",   interactShow Advent03.day3)
    , ("3b",  interactShow Advent03.day3b)
    , ("4",   interactShow Advent04.day4)
    , ("4b",  interactShow Advent04.day4b)
    , ("5",   interact Advent05.day5)
    , ("5b",  interact Advent05.day5b)
    , ("6",   interact Advent06.day6)
    , ("6b",  interact Advent06.day6b)
    , ("7",   interact Advent07.day7)
    , ("7b",  interact Advent07.day7b)
    , ("8",   interact Advent08.day8)
    , ("8b",  interact Advent08.day8b)
    , ("9",   interact Advent09.day9)
    , ("9b",  interact Advent09.day9b)
    , ("10",  interact Advent10.day10)
    , ("10b", interact Advent10.day10b)
    , ("11",  interact Advent11.day11)
    , ("11b", interact Advent11.day11b)
    , ("12",  interact Advent12.day12)
    , ("12b", interact Advent12.day12b)
    , ("13",  interact Advent13.day13)
    , ("13b", interact Advent13.day13b)
    , ("14",  interact Advent14.day14)
    , ("14b", interact Advent14.day14b)
    , ("15",  print    Advent15.day15)
    , ("15b", print    Advent15.day15b)
    , ("16",  interact Advent16.day16)
    , ("16b", interact Advent16.day16b)
    , ("17",  interact Advent17.day17)
    , ("17b", interact Advent17.day17b)
    , ("18",  interact Advent18.day18)
    , ("18b", interact Advent18.day18b)
    , ("19",  interact Advent19.day19)
    , ("19b", interact Advent19.day19b)
    , ("20",  interact Advent20.day20)
    , ("20b", interact Advent20.day20b)
    , ("21",  interact Advent21.day21)
    , ("21b", interact Advent21.day21b)
    , ("22",  interact Advent22.day22)
    , ("22b", interact Advent22.day22b)
    , ("23",  interact Advent23.day23)
    , ("23b",          Advent23.day23b)
    , ("24",  interact Advent24.day24)
    , ("24b", interact Advent24.day24b)
    , ("25",  interact Advent25.day25)

    , ("19ReadP",  interact Advent19ReadP.day19)
    , ("19ReadPb", interact Advent19ReadP.day19b)
    ]

help :: a
help = error $ "Usage: advent2020 (" ++ intercalate " | " (map fst days) ++ ")"

main :: IO ()
main = do
    as <- getArgs
    case as of
        [d] -> fromMaybe help (lookup d days)
        _   -> help

foo :: Eq a => (a->a)->a->a
foo f = fst . until (uncurry (==)) ((f &&& id) . fst) . (f&&&id)

-- >>> foo (fromInteger . floor) (9.9 :: Double)
-- 9.9

