------------------------------------------------------------------------------
-- If we list all the natural numbers below 10 that are multiples of 3
-- or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.
------------------------------------------------------------------------------

import System.Environment (getArgs)

-- Sum of multiplies of 'm' is '1 * m + 2 * m + ... + n * m'.
-- This could be rewritten as 'm * (1 + 2 + ... + n)'.
-- Also '1 + 2 + .. + n = (n * (n + 1)) `div` 2', thus
-- 'm * (n * (n + 1) `div` 2)'.
sumUpTo k m =
  m * (n * (n + 1) `div` 2)
  where
    n = (k - 1) `div` m

-- Run as ./0001 1000 3 5
main = do
  (n : muls) <- getArgs >>= mapM readIO
  print $ sum (map (sumUpTo n) muls) - sumUpTo n (product muls)
