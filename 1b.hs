import Prelude

main :: IO ()
main = do
  input <- getContents
  (print . fn . map read . lines) input

doCount :: (Int, Int) -> [Int] -> Int
doCount (n, prev) (x : y : z : rest) = doCount (next, current) (y : z : rest)
  where
    current = x + y + z
    next = if prev < current then n + 1 else n
doCount (n, prev) _ = n

fn :: [Int] -> Int
fn = doCount (-1, -1)
