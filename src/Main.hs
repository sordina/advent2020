module Main where

import Data.Maybe
import System.Environment ( getArgs )
import Data.List (intercalate)

import qualified Advent01
import qualified Advent02
import qualified Advent03
import qualified Advent04

days :: [(String, IO ())]
days =
    [ ("1",  interact Advent01.day1)
    , ("2",  interact Advent02.day2)
    , ("2b", interact Advent02.day2b)
    , ("3",  interact Advent03.day3)
    , ("3b", interact Advent03.day3b)
    , ("4",  interact Advent04.day4)
    , ("4b", interact Advent04.day4b)
    ]

help :: a
help = error $ "Usage: advent2020 (" ++ intercalate " | " (map fst days) ++ ")"

main :: IO ()
main = do
    as <- getArgs
    case as of
        [d] -> fromMaybe help (lookup d days)
        _   -> help
