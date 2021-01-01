module Advent08 where

import Control.Lens
import Control.Monad.State
import Data.Maybe (fromMaybe, fromJust)
import Control.Arrow (first, Arrow((&&&)))

day8 = show . view _2 . (\x -> (0,0,[]) &~ let g = (zoom _1 get >>= \p -> zoom _3 (gets (elem p)) >>= \q -> _3 %= (p:) >> when (not q) (let (a,b) = x!!p in fromJust (lookup a t) b >> g)) in g) . map (\[i,v] -> (i, read (dropWhile (flip elem "+") v))) . map words . lines
    where
    t = [("nop", const (_1 %= succ)), ("acc", \n -> (_2 %= (+n)) >> (_1 %= succ)), ("jmp", \j -> _1 %= (+j))]

day8b = show . map (view _2) . filter (\(a,b,c,l) ->a >=l) . map (\(l,x) -> (0,0,0,l) &~ let g = (zoom _1 get >>= \p -> zoom _3 get >>= \q -> _3 %= succ >> when (q<l&&p<l) (let (a,b) = x!!p in fromJust (lookup a t) b >> g)) in g) . (\(l,p) -> map (\d-> (l, zipWith ($) (replicate d id ++ [first (\z -> fromMaybe z $ lookup z [("nop","jmp"),("jmp","nop")])] ++ repeat id) p)) [0.. l]) . (length &&& id) . map (\[i,v] -> (i, read (dropWhile (flip elem "+") v))) . map words . lines
    where
    t = [("nop", \_a -> (_1 %= succ)), ("acc", \n -> (_2 %= (+n)) >> (_1 %= succ)), ("jmp", \j -> _1 %= (+j))]

