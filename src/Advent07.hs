module Advent07 where

-- hoe2 'length.nub.concat.map snd.takeWhile (uncurry (/=)).uncurry zip.(id &&& tail).(\x -> iterate (\y -> nub [ h|(h,t)<-x,z<-y,t==z]) [T.pack "shiny gold"]).concatMap (\(h,t) -> [(j,v)|i<-h,j<-i,u<-t,v<-u]).map((^.. regex [rx|^(\w+\s\w+) bags|] . groups)&&& (^.. regex [rx|\d\s(\w+\s\w+)\sbag|] . groups)).T.lines.T.pack'

-- #!/bin/sh
-- hoe2 'sum.map snd.concat.tail.takeWhile (not . null).(\x -> iterate (\y -> [(t,n*o)|(h,t,n)<-x,(z,o)<-y,h==z]) [(T.pack "shiny gold",1)]).concatMap (\([[h]],t) -> [(h,v,read (T.unpack n) :: Int)|[n,v]<-t]).map((^.. regex [rx|^(\w+\s\w+) bags|] . groups)&&& (^.. regex [rx|\s(\d+)\s(\w+\s\w+)\sbag|] . groups)).T.lines.T.pack'

day7 = undefined

day7b = undefined

