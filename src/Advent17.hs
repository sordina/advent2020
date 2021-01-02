module Advent17 where

import qualified Data.Set as S
import Control.Arrow (Arrow((&&&)))

day17
    = show . S.size . (!! 6) . iterate step . S.fromList
    . (\ls -> [(x,y,0) | (y,l) <- zip [0..] ls, (x,c) <- zip [0..] l, c `elem` "#"])
    . lines

    where
    around a b = [pred a .. succ b]

    n p@(x,y,z) = [(x1,y1,z1) | x1 <- f x, y1 <- f y, z1 <- f z, p /= (x1,y1,z1)]
        where
        f m = around m m

    extents l = [(x,y,z)| x<-f xs, y<-f ys, z<-f zs]
        where
        ls  = S.elems l
        xs  = map (\(x,_,_) -> x) ls
        ys  = map (\(_,y,_) -> y) ls
        zs  = map (\(_,_,z) -> z) ls
        f m = around (minimum m) (maximum m)

    t True b
        | b `elem` [2,3] = True
        | otherwise      = False
    t False 3            = True
    t _     _            = False

    step p = S.fromList $ concatMap (gen p . (id &&& n)) (extents p)

    gen s (p,l) = [p | t (S.member p s) (length $ filter id $ map (`S.member` s) l)]

day17b
    = show . S.size . (!! 6) . iterate step . S.fromList
    . (\ls -> [(x,y,0,0) | (y,l) <- zip [0..] ls, (x,c) <- zip [0..] l, c `elem` "#"])
    . lines

    where
    around a b = [pred a .. succ b]

    n p@(x,y,z,k) = [(x1,y1,z1,k1) | x1<-f x, y1<-f y, z1<-f z, k1<-f k, p /= (x1,y1,z1,k1)]
        where
        f m = around m m

    extents l = [(x,y,z,k)| x<-f xs, y<-f ys, z<-f zs, k<-f ks]
        where
        ls  = S.elems l
        xs  = map (\(x,_,_,_) -> x) ls
        ys  = map (\(_,y,_,_) -> y) ls
        zs  = map (\(_,_,z,_) -> z) ls
        ks  = map (\(_,_,_,k) -> k) ls
        f m = around (minimum m) (maximum m)

    t True b | b `elem` [2,3] = True
                | otherwise    = False
    t False 3               = True
    t _     _               = False

    step p = S.fromList $ concatMap (gen p . (id &&& n)) (extents p)

    gen s (p,l) = [p | t (S.member p s) (length $ filter id $ map (`S.member` s) l)]
