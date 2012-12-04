------------------------------------------------------------------------------
-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all
-- of the numbers from 1 to 20?
------------------------------------------------------------------------------

main = print $ head [n | n <- [20,40..], all (\m -> mod n m == 0) [19,18..11]]
-- main = print $ foldl1 lcm [1..20]
