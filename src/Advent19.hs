module Advent19 where

-- See http://sordina.github.io/blog/2021/01/03/1609638326-advent19b.html for writeup.
-- https://hackage.haskell.org/package/yoctoparsec-0.1.0.0
-- https://hackage.haskell.org/package/parser-combinators
-- 
-- Also implemented in Advent19ReadP.hs - Uses ReadP from base.

import Data.Either (rights)
import Text.Parsec (try, choice, string, eof, parse, Parsec)
import Control.Arrow (second, Arrow((&&&)))
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.Maybe (fromJust)

import Control.Monad.Trans.Free ( FreeT(FreeT), iterTM, FreeF(Pure, Free) )
import Data.Tree ( Tree(Node), drawTree )
import Control.Monad.State ( mfilter, StateT(StateT, runStateT) )
import Control.Lens ( (&), (.~), Field2(_2) )
import Control.Applicative ( Alternative(empty, (<|>)) )

import qualified Data.List as L
import qualified Data.Map  as M

import Control.Monad.Yoctoparsec
import qualified Control.Applicative.Combinators as PC

-- The received messages (the bottom part of your puzzle input) need to be
-- checked against the rules so you can determine which are valid and which are
-- corrupted.

day19
    = show . length .  rights
    . (\(s,p) -> map (parse p "rules") s)
    . (last &&& foo . map ((init . head &&& splitOn ["|"] . tail) . words) .  head)
    . splitOn [""] . lines
    where
    foo :: [(String,[[String]])] -> Parsec String () ()
    foo r = a ! "0" *> eof
      where
      a = M.fromList (map g r)
      f n@(x:xs)
        | isDigit x = a ! n
        | otherwise = () <$ string (init xs)
      g = second (choice . map (try . mapM_ f))
      m ! k = fromJust $ M.lookup k m

-- As you look over the list of messages, you realize your matching rules aren't
-- quite right. To fix them, completely replace rules 8: 42 and 11: 42 31 with
-- the following:
-- 
-- 8: 42 | 42 8
-- 11: 42 31 | 42 11 31

day19b
    -- = concat . (\x -> map drawTree x ++ [show $ length x])
    = show . length
    . map (fst . head)
    . filter (not . null)
    . (\(s,p) -> map (parseString p) s)
    . (map (++"EOF") . last &&& build . map (sub . (init . head &&& splitOn ["|"] . tail) . words) .  head)
    . splitOn [""] . lines

    where

    char c = mfilter (==c) token
    string = mapM char

    sub t@("8", _) = t & _2 .~ [["42"],["42","8"]]
    sub t@("11",_) = t & _2 .~ [["42","31"],["42","11","31"]]
    sub t          = t

    m ! k = fromJust $ M.lookup k m

    build :: [(String,[[String]])] -> FreeT ((->) Char) [] (Tree String)
    build r = fmap (Node "0") $ a ! "0" <* string "EOF"
        where
        a = M.fromList (map (second g) r)
        g = PC.choice . map (traverse f)
        f n@(x:xs)
            | isDigit x  = Node n <$> a ! n
            | otherwise  = Node n [] <$ string (init xs)

-- >>> day19 exampleInput
-- "2"

-- "Your goal is to determine the number of messages that completely match rule
--  0. In the above example, ababbb and abbbab match, but bababa, aaabbb, and
--  aaaabbb do not, producing the answer 2."

-- >>> day19b exampleInputB
-- "12"

-- After updating rules 8 and 11, a total of 12 messages match:

exampleInput = unlines
  [ "0: 4 1 5"
  , "1: 2 3 | 3 2"
  , "2: 4 4 | 5 5"
  , "3: 4 5 | 5 4"
  , "4: \"a\""
  , "5: \"b\""
  , ""
  , "ababbb"
  , "bababa"
  , "abbbab"
  , "aaabbb"
  , "aaaabbb"
  ]

exampleInputB = unlines
  [ "42: 9 14 | 10 1"
  , "9: 14 27 | 1 26"
  , "10: 23 14 | 28 1"
  , "1: \"a\""
  , "11: 42 31"
  , "5: 1 14 | 15 1"
  , "19: 14 1 | 14 14"
  , "12: 24 14 | 19 1"
  , "16: 15 1 | 14 14"
  , "31: 14 17 | 1 13"
  , "6: 14 14 | 1 14"
  , "2: 1 24 | 14 4"
  , "0: 8 11"
  , "13: 14 3 | 1 12"
  , "15: 1 | 14"
  , "17: 14 2 | 1 7"
  , "23: 25 1 | 22 14"
  , "28: 16 1"
  , "4: 1 1"
  , "20: 14 14 | 1 15"
  , "3: 5 14 | 16 1"
  , "27: 1 6 | 14 18"
  , "14: \"b\""
  , "21: 14 1 | 1 14"
  , "25: 1 1 | 1 14"
  , "22: 14 14"
  , "8: 42"
  , "26: 14 22 | 1 20"
  , "18: 15 15"
  , "7: 14 5 | 1 21"
  , "24: 14 1"
  , ""
  , "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"
  , "bbabbbbaabaabba"
  , "babbbbaabbbbbabbbbbbaabaaabaaa"
  , "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
  , "bbbbbbbaaaabbbbaaabbabaaa"
  , "bbbababbbbaaaaaaaabbababaaababaabab"
  , "ababaaaaaabaaab"
  , "ababaaaaabbbaba"
  , "baabbaaaabbaaaababbaababb"
  , "abbbbabbbbaaaababbbbbbaaaababb"
  , "aaaaabbaabaaaaababaa"
  , "aaaabbaaaabbaaa"
  , "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
  , "babaaabbbaaabaababbaabababaaab"
  , "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
  ]