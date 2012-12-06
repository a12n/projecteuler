module Common.TriangNum ( checkTriNum
                        , triNum ) where

-- | Just 'n' if 'm' is 'n'th triangular number, or Nothing.
checkTriNum :: Integral a => a -> Maybe a
checkTriNum m =
  if c == f then
    Just c
  else
    Nothing
  where
    -- It's triangular number if there is positive integral solution for:
    -- 1/2 n(n + 1) = m
    -- n(n + 1) = 2m
    -- n^2 + n = 2m
    -- n^2 + n - 2m = 0
    d = sqrt (fromIntegral (1 + 8 * m))
    x = (-1 + d) / 2
    c = ceiling x
    f = floor x

-- | 'n'th triangular number 'm'.
triNum :: Integral a => a -> a
triNum n = n * (n + 1) `div` 2
