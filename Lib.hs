module Lib where

import Prelude
import Data.List (foldl')
import Data.Char (digitToInt)

bitStringToDec :: String -> Int
bitStringToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

multiplyTuple :: (Int, Int) -> Int
multiplyTuple = uncurry (*)

dropThird :: (a, b, c) -> (a, b)
dropThird (x, y, _) = (x, y)
