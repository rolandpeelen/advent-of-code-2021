import Data.Bifunctor
import Data.List
import Lib
import Prelude

main :: IO ()
main = do
  input <- getContents
  ( print
      . product
      . map bitStringToDec
      . transpose
      . map (setBits . countBits)
      . transpose
      . lines
    )
    input

countBits :: [Char] -> (Int, Int)
countBits xs = (count '0' xs, count '1' xs)

setBits :: (Int, Int) -> [Char]
setBits (zeros, ones) = [gamma, epsilon]
  where
    gamma
      | zeros >= ones = '0'
      | otherwise = '1'
    epsilon
      | zeros >= ones = '1'
      | otherwise = '0'
