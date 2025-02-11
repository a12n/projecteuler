module Common.Prime ( divides
                    , divisors
                    , isPrime
                    , leastDivisor
                    , primeFactors
                    , primeNums
                    , primeNumsTo ) where

divides :: Integral a => a -> a -> Bool
divides m n = mod n m == 0

-- XXX: Not exactly related to primes.
divisors :: Integral a => a -> [a]
divisors 1 = []
divisors 2 = [1]
divisors n | even n    = 1 : aux 2 1
           | otherwise = 1 : aux 3 2
  where
    aux k s | k^2 > n   = []
            | otherwise =
      case divMod n k of
        (m, 0) ->
          if m /= k then
            (k : m : aux (k + s) s)
          else
            (k : aux (k + s) s)
        other ->
          aux (k + s) s

isPrime :: Integral a => a -> Bool
isPrime n = leastDivisor n == n

primeNums :: Integral a => [a]
primeNums = 2 : 3 : filter isPrime [4..]

leastDivisor :: Integral a => a -> a
leastDivisor n = findIn primeNums
  where
    findIn (p : ps) | divides p n = p
                    | p^2 > n     = n
                    | otherwise   = findIn ps

primeFactors :: Integral a => a -> [a]
primeFactors 1 = []
primeFactors n = f : primeFactors (n `div` f)
  where
    f = leastDivisor n

primeNumsTo :: Integral a => a -> [a]
primeNumsTo n = sieve (2 : [3,5..n])
  where
    sieve [] = []
    sieve ps@(p : ns)
      | p^2 > n   = ps
      | otherwise = p : sieve (filter (\x -> not (divides p x)) ns)
