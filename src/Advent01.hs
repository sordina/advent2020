
module Advent01 where

day1  = show . head . (\x -> [a*b*c | a<-x, b<-x, c<-x, a+b+c == 2020]) . map read . words
