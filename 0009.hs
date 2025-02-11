------------------------------------------------------------------------------
-- A Pythagorean triplet is a set of three natural numbers, a < b < c,
-- for which,
-- a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
------------------------------------------------------------------------------

main = print $ a * b * c
  where
    (a, b, c) = head $ [(a, b, 1000 - a - b) | a <- [1..995],
                                               b <- [(a + 1)..996],
                                               a^2 + b^2 == (1000 - a - b)^2]
