module Advent23 where

-- TODO: Import Vector
-- import Data.Vector         as V
-- import Data.Vector.Mutable as MV

import Data.Maybe (fromJust)
import Data.List (elemIndex, sort)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

day23
    = (++"\n") . (!! 100)
    . map (
        concatMap show
        . (\l -> take (pred $ length l) $ tail $ dropWhile (/= 1) $ cycle l)
    )
    . iterate move
    . map (read . return) . head . words

    where

    d n a b l h
        | n `elem` a = d (pred n) a b l h
        | n < l      = d h a b l h
        | otherwise  = n

    rl n c = take l $ drop n (cycle c) where l = length c

    move c@(x:xs) = rl 1 $ e ++ a ++ f
        where
        (a,b)  = splitAt 3 xs
        ys     = x:b
        n      = d (pred x) a ys (minimum c) (maximum c)
        (e,f)  = splitAt (succ $ fromJust $ elemIndex n ys) ys

day23b = do
  l@(n:_) <- Prelude.map (Prelude.read . return) . Prelude.head . words <$> getLine
  let v = prep l
  m <- V.thaw v
  _ <- iterateM_ (10000000 :: Int) (move (V.minimum v) (V.maximum v) m) n
  f <- V.freeze m
  print $ look f

  where

    iterateM_ 0 _ i = return i
    iterateM_ n f i = f i >>= iterateM_ (pred n) f

    d n a l h
        | n `elem` a = d (pred n) a l h
        | n < l      = d h a l h
        | otherwise  = n

    move l h v i = do
        a_ <- MV.read v i
        b_ <- MV.read v a_
        c_ <- MV.read v b_
        d_ <- MV.read v c_
        let n = d (pred i) [a_,b_,c_] l h
        o <- MV.read v n
        MV.write v i d_
        MV.write v n a_
        MV.write v c_ o
        MV.read  v i

    look l = a * b
        where
        a = l V.! 1
        b = l V.! a

    prep
        = V.fromList
        . Prelude.map snd
        . sort
        . (\l -> Prelude.zip l (Prelude.tail l))
        . (\l -> 0 : l ++ [succ (Prelude.maximum l) .. 1000000] ++ [Prelude.head l])
