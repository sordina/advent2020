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

f x = length . concatMap (filter x . (^.. rx . groups)) . T.lines . T.pack

-- >>> day2 exampleInput
-- 2

-- "In the above example, 2 passwords are valid."

-- >>> day2b exampleInput
-- 1

-- Given the same example list from above:
-- 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
-- 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
-- 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.


exampleInput =
    "1-3 a: abcde\
    \1-3 b: cdefg\
    \2-9 c: ccccccccc"
