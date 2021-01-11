module Advent19 where

-- See http://sordina.github.io/blog/2021/01/03/1609638326-advent19b.html for writeup.
-- https://hackage.haskell.org/package/parser-combinators

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
