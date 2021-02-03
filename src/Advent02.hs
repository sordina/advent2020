{-# LANGUAGE QuasiQuotes #-}

module Advent02 where

import Control.Lens.Regex.Text ( groups, regex )
import qualified Data.Text as T
import Control.Lens ( (^..) )


-- "Each line gives the password policy and then the password. The password
--  policy indicates the lowest and highest number of times a given letter must
--  appear for the password to be valid. For example, 1-3 a means that the
--  password must contain a at least 1 time and at most 3 times."

day2 = f f2
    where
    f2 [l,h,c,p] = n >= read (T.unpack l) && n <= read (T.unpack h)
        where
        n = T.length (T.filter (== T.index c 0) p)

-- "Each policy actually describes two positions in the password, where 1 means
--  the first character, 2 means the second character, and so on. (Be careful;
--  Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of
--  these positions must contain the given letter. Other occurrences of the
--  letter are irrelevant for the purposes of policy enforcement."

day2b = f f2b
    where
    f2b [l,h,c,p]
        = 1 == length (
            filter (== T.index c 0)
            [T.index p (pred $ read (T.unpack l)), T.index p (pred $ read (T.unpack h))])

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
