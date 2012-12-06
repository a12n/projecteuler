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

data WeekDay = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Enum, Eq, Show)

type Date = (Int, Int, Int)

type JDN = Int

fromDate :: Date -> JDN
fromDate (y, m, d) =
  d + (153 * m' + 2) `div` 5 + 365 * y' + y' `div` 4 - g'
  where
    a' = (14 - m) `div` 12
    y' = y + 4800 - a'
    m' = m + 12 * a' - 3
    g' | (y, m, d) > (1582, 10, 15) = y' `div` 100 - y' `div` 400 + 32045
       | otherwise                  = 32083

fromJDN :: JDN -> WeekDay
fromJDN jdn = toEnum $ mod (jdn + 1) 7

main = print $ sum [1 | y <- [1901..2000], m <- [1..12], fromJDN (fromDate (y, m, 1)) == Sun]
