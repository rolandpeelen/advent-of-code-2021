import Prelude

main :: IO ()
main = do
  input <- getContents
  (print . fn . map read . lines) input

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

fn :: [Int] -> Int
fn xs = count True $ zipWith (>) (tail xs) xs
