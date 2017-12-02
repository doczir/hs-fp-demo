module D_LazyEval where

import           Debug.Trace

nat = [0..]

n29 = nat !! 29
nf1000 = take 1000 nat

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


fibsd = 0 : 1 : zipWith add fibsd (tail fibsd)
  where
    add a b = trace dbgStr result
      where
        result = a + b
        dbgStr = show a ++ "+" ++ show b ++ "=" ++ show result

-- only throws when it actually calculates div by zero
divBy num = map (num `div`)
