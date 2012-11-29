module Common.NumTriangle ( NumTriangle
                          , fromElements
                          , maxTotal ) where

import Data.Array.Unboxed (UArray, (!), (//), listArray)

type NumTriangle = UArray Int Int

baseIndex :: Int -> Int
baseIndex n = (nElements n) - n + 1

fromElements :: Int -> [Int] -> NumTriangle
fromElements n = listArray (1, nElements n)

nElements :: Int -> Int
nElements n = (n * (n + 1)) `div` 2

maxTotal :: NumTriangle -> Int -> Int
maxTotal a 1 = a ! 1
maxTotal a n = maxTotal (a // upd) (n - 1)
  where
    upd = aux [0..(n - 2)]
    aux [] = []
    aux (i : is) = (ixP, elP + (max el1 el2)) : aux is
      where
        ixP = baseIndex (n - 1) + i
        ix1 = baseIndex n + i
        ix2 = ix1 + 1
        elP = a ! ixP
        el1 = a ! ix1
        el2 = a ! ix2
