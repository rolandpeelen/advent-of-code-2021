import Prelude

main :: IO ()
main = do
  input <- getContents
  (putStr . show . fn . lines) input

fn :: a -> a
fn = id
