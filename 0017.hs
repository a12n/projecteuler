------------------------------------------------------------------------------
-- If the numbers 1 to 5 are written out in words: one, two, three, four,
-- five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were
-- written out in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
-- and forty-two) contains 23 letters and 115 (one hundred and fifteen)
-- contains 20 letters. The use of "and" when writing out numbers is in
-- compliance with British usage.
------------------------------------------------------------------------------

import Data.Char (isAlpha)

s1  1 = "one"
s1  2 = "two"
s1  3 = "three"
s1  4 = "four"
s1  5 = "five"
s1  6 = "six"
s1  7 = "seven"
s1  8 = "eight"
s1  9 = "nine"
s1 10 = "ten"
s1 11 = "eleven"
s1 12 = "twelve"
s1 13 = "thirteen"
s1 14 = "fourteen"
s1 15 = "fifteen"
s1 16 = "sixteen"
s1 17 = "seventeen"
s1 18 = "eighteen"
s1 19 = "nineteen"
s1  _ = ""

s10 n =
  if q > 1 then
    (p10 q) ++ "ty" ++ (if r > 0 then "-" ++ (s1 r) else "")
  else
    s1 n
  where
    (q, r) = divMod n 10
    p10 2 = "twen"
    p10 3 = "thir"
    p10 4 = "for"
    p10 5 = "fif"
    p10 6 = "six"
    p10 7 = "seven"
    p10 8 = "eigh"
    p10 9 = "nine"

s100 n =
  if q > 0 then
    (s1 q) ++ " hundred" ++ (if r > 0 then " and " ++ (s10 r) else "")
  else
    s10 r
  where
    (q, r) = divMod n 100

s1000 n =
  if q > 0 then
    (s1 q) ++ " thousand" ++ (if r > 0 then " " ++ (s100 r) else "")
  else
    s100 r
  where
    (q, r) = divMod n 1000

spell = s1000

main = print $ length (concat (map (filter isAlpha . spell) [1..1000]))
