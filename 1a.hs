import Prelude
import Lib

main :: IO ()
main = do
  input <- getContents
  (print . fn . map read . lines) input

fn :: [Int] -> Int
fn xs = count True $ zipWith (>) (tail xs) xs
