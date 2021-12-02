module Lib where

import Prelude

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

multiplyTuple :: (Int, Int) -> Int
multiplyTuple = uncurry (*)

dropThird :: (a, b, c) -> (a, b)
dropThird (x, y, _) = (x, y)
