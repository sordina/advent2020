{-# LANGUAGE QuasiQuotes #-}

module Advent02 where

import Control.Lens.Regex.Text ( groups, regex )
import qualified Data.Text as T
import Control.Lens ( (^..) )

day2  = f f2
    where
    f2 [l,h,c,p] = n >= read (T.unpack l) && n <= read (T.unpack h) where n = T.length (T.filter (== T.index c 0) p)

day2b = f f2b
    where
    f2b [l,h,c,p] = 1 == length (filter (== T.index c 0) [T.index p (pred $ read (T.unpack l)), T.index p (pred $ read (T.unpack h))])

rx = [regex|(\d+)-(\d+) (\S+): (\S+)|]

f x = show . length . concatMap (filter x . (^.. rx . groups)) . T.lines . T.pack