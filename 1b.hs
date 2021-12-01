-- 1 2 3 4 5
-- _____ x1
--   _____ x2
--      _____ x3
-- diff between x1 and x2 is 1 / 4
-- diff between x2 and x3 is 2 / 5

import Prelude

main :: IO ()
main = do
  input <- getContents
  (print . fn . map read . lines) input

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

fn :: [Int] -> Int
fn xs = count True $ zipWith (>) (drop 3 xs) xs
