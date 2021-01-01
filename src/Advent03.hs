
module Advent03 where

day3 = show . length . filter id . f . map cycle . lines
    where
    f m = zipWith (\x y -> m!!y!!x == '#') [0,3..] [0..length m-1] 


day3b
    = show . product . f g . map cycle . lines
    where
    f z m = [sum[1|True<-zipWith(\x y->m!!y!!x=='#')i(takeWhile(<length m)j)]|(i,j)<-[z 1,z 3,z 5,z 7,([0..],[0,2..])]]
    g z = ([0,z..],[0..])
