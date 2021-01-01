{-# LANGUAGE QuasiQuotes #-}

module Advent02 where

import Control.Lens.Regex.Text ( groups, regex )
import qualified Data.Text as T
import Control.Lens ( (^..) )

rx = [regex|(\d+)-(\d+) (\S+): (\S+)|]

day2, day2b :: String -> String

day2  = f f2
day2b = f f2b

f x = show . length . concatMap (filter x) . map (^.. rx . groups) . T.lines . T.pack

f2 [l,h,c,p] = n >= read (T.unpack l) && n <= read (T.unpack h)
    where
    n = T.length (T.filter (== T.index c 0) p)

f2b [l,h,c,p] = 1 == length (filter (== T.index c 0) [T.index p (pred $ read (T.unpack l)), T.index p (pred $ read (T.unpack h))])