module Advent23 where

-- TODO: Import Vector
-- import Data.Vector         as V
-- import Data.Vector.Mutable as MV

import Data.Maybe (fromJust)
import Data.List (elemIndex)

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

day23b = undefined

{-

hoe2 -m 'Data.Vector Data.Vector.Mutable' '

let

readMV    = Data.Vector.Mutable.read
writeMV   = Data.Vector.Mutable.write
freezeV   = Data.Vector.freeze
thawV     = Data.Vector.thaw
maximumV  = Data.Vector.maximum
minimumV  = Data.Vector.minimum
fromListV = Data.Vector.fromList
(+++)     = (Prelude.++)
(!!!)     = (Data.Vector.!)

iterateM_ 0 _ i = return i
iterateM_ n f i = f i >>= iterateM_ (pred n) f

d n a l h
	| Prelude.elem n a = d (pred n) a l h
	| n < l            = d h a l h
	| otherwise        = n

move l h v i = do
	a_ <- readMV v i
	b_ <- readMV v a_
	c_ <- readMV v b_
	d_ <- readMV v c_
	let n = d (pred i) [a_,b_,c_] l h
	o <- readMV v n
	writeMV v i d_
	writeMV v n a_
	writeMV v c_ o
	readMV v i

look l = a * b
	where
	a = l !!! 1
	b = l !!! a

prep
	= fromListV
	. Prelude.map snd
	. sort
	. (\l -> Prelude.zip l (Prelude.tail l))
	. (\l -> 0 : l +++ [succ (Prelude.maximum l) .. 1000000] +++ [Prelude.head l])

in
do
	l@(n:_) <- Prelude.map (Prelude.read . return) . Prelude.head . words <$> getLine
	let v = prep l
	m <- thawV v
	_ <- iterateM_ (10000000 :: Int) (move (minimumV v) (maximumV v) m) n
	f <- freezeV m
	print $ look f
'

-}