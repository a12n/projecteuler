module Common.Primes ( isDivis
                     , isPrime
                     , leastDivisor
                     , primeFactors
                     , primeNums ) where

isDivis :: Integral a => a -> a -> Bool
isDivis n m = mod n m == 0

isPrime :: Integral a => a -> Bool
isPrime n = leastDivisor n == n

primeNums :: Integral a => [a]
primeNums = 2 : 3 : filter isPrime [4..]

leastDivisor :: Integral a => a -> a
leastDivisor n = findIn primeNums
  where
    findIn (p : ps) | isDivis n p = p
                    | p^2 > n     = n
                    | otherwise   = findIn ps

primeFactors :: Integral a => a -> [a]
primeFactors 1 = []
primeFactors n = f : primeFactors (n `div` f)
  where
    f = leastDivisor n
