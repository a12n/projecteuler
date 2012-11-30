module Common.TriangNum ( triNum ) where

-- | 'n'th triangular number.
triNum :: Integral a => a -> a
triNum n = n * (n + 1) `div` 2

-- triRt x = (sqrt (8 * x + 1) - 1) / 2
