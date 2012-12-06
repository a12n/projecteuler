module Common.String ( alphPos
                     , wordVal ) where

import Data.Char (ord)

alphPos :: Char -> Int
alphPos c = ord c - ord 'A' + 1

wordVal :: String -> Int
wordVal s = sum (map alphPos s)
