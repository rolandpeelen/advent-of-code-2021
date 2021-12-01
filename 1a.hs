import Prelude

main :: IO ()
main = do
  input <- getContents
  (print . fn . map read . lines) input

countIncreases :: (Int, Int) -> Int -> (Int, Int)
countIncreases (prev, n) x = (x, next)
  where
    next = if prev < x then n + 1 else n

fn :: [Int] -> Int
fn =  snd . foldl countIncreases (-1, -1)
