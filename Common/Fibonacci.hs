module Common.Fibonacci ( fibNums
                        , fibNumsFrom ) where

fibNums :: Num a => [a]
fibNums = fibNumsFrom 0 1

fibNumsFrom :: Num a => a -> a -> [a]
fibNumsFrom a b = a : b : aux a b
  where
    aux a b = c : aux b c
      where
        c = a + b
