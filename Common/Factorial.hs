module Common.Factorial (fac) where

fac :: Integral a => a -> a
fac n = aux 1 n
  where
    aux p q | (p == q)  = p
            | otherwise = (aux p m) * (aux (m + 1) q)
      where
        m = p `div` 2 + q `div` 2
