{-# LANGUAGE QuasiQuotes #-}

module Advent07 where

import Control.Lens.Regex.Text (groups, regex)
import qualified Data.Text as T
import Data.List (nub)
import Control.Arrow (Arrow((&&&)))
import Control.Lens ( (^..) )

day7 = show . length . nub . concat . map snd . takeWhile (uncurry (/=)) . uncurry zip . (id &&& tail) . (\x -> iterate (\y -> nub [ h|(h,t)<-x,z<-y,t==z]) [T.pack "shiny gold"]) . concatMap (\(h,t) -> [(j,v)|i<-h,j<-i,u<-t,v<-u]) . map((^.. [regex|^(\w+\s\w+) bags|] . groups) &&& (^.. [regex|\d\s(\w+\s\w+)\sbag|] . groups)).T.lines.T.pack

day7b = show . sum.map snd.concat.tail.takeWhile (not . null).(\x -> iterate (\y -> [(t,n*o)|(h,t,n)<-x,(z,o)<-y,h==z]) [(T.pack "shiny gold",1)]).concatMap (\([[h]],t) -> [(h,v,read (T.unpack n) :: Int)|[n,v]<-t]).map((^.. [regex|^(\w+\s\w+) bags|] . groups)&&& (^.. [regex|\s(\d+)\s(\w+\s\w+)\sbag|] . groups)).T.lines.T.pack

