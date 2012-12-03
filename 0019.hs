------------------------------------------------------------------------------
-- You are given the following information, but you may prefer to do some
-- research for yourself.
--
--   * 1 Jan 1900 was a Monday.
--   * Thirty days has September,
--     April, June and November.
--     All the rest have thirty-one,
--     Saving February alone,
--     Which has twenty-eight, rain or shine.
--     And on leap years, twenty-nine.
--   * A leap year occurs on any year evenly divisible by 4, but not on a
--     century unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth
-- century (1 Jan 1901 to 31 Dec 2000)?
------------------------------------------------------------------------------

import Common.Prime (divides)
import Data.List (intersect)

leap year = divides 400 year || (divides 4 year && not (divides 100 year))

days year | leap year = [ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
          | otherwise = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

main = print $ length $ intersect fsts suns
  where
    years = [1901..2000]
    fsts = init $ scanl (+) 1 (concat (map days years))
    suns = takeWhile (<= (last fsts)) [6,(6 + 7)..]
