------------------------------------------------------------------------------
-- The following iterative sequence is defined for the set of positive
-- integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following
-- sequence: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz
-- Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one
-- million.
------------------------------------------------------------------------------

-- TODO: Use cache array.
main = print $ maximum (zip (map nWondr [1..999999]) [1..] :: [(Int, Int)])
  where
    nWondr n = aux n 0
      where
        aux 1 ans = ans + 1
        aux n ans | even n    = aux (n `div` 2) (ans + 1)
                  | otherwise = aux (3 * n + 1) (ans + 1)
