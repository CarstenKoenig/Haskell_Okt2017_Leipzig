module Main where

import Control.Parallel
import Data.List

-- I've taken this staright from [this great answer](https://stackoverflow.com/a/19119503/76051) on Stack-Overflow

pfold :: (Num a, Enum a) => (a -> a -> a) -> [a] -> a
pfold _ [x] = x
pfold mappend xs  = (ys `par` zs) `pseq` (ys `mappend` zs) where
  len = length xs
  (ys', zs') = splitAt (len `div` 2) xs
  ys = pfold mappend ys'
  zs = pfold mappend zs'

-- compile with stack -- ghc -O2 -threaded Monoids.hs
-- and run with either +RTS -N1 -s or +RTS -N2 -s
-- +RTS (RunTimeSystem - see [here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html))


main :: IO ()
main =
  print $ pfold (+) [ foldl' (*) 1 [1..x] | x <- [1..5000] ]
