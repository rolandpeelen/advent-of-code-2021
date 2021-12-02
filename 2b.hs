import Data.Bifunctor
import Data.Either
import Lib
import Text.Parsec hiding (count)
import Prelude

data Instruction = Forward Int | Down Int | Up Int deriving (Show)

type Traveled = Int

type Depth = Int

type Aim = Int

type Position = (Traveled, Depth, Aim)

directionP :: Parsec String () Instruction
directionP = forward <|> down <|> up
  where
    forward = string "forward" *> (Forward <$> number)
    down = string "down" *> (Down <$> number)
    up = string "up" *> (Up <$> number)
    number = read <$> (space *> many1 digit)

main :: IO ()
main = do
  input <- getContents
  ( print
      . multiplyTuple
      . dropThird
      . applyInstructions (0, 0, 0)
      . rights
      . map (runParser directionP () "")
      . lines
    )
    input

applyInstructions :: Position -> [Instruction] -> Position
applyInstructions = foldl (flip applyInstruction)
  where
    applyInstruction (Forward x) (t, d, a) = (t + x, d + (x * a), a)
    applyInstruction (Down x) (t, d, a) = (t, d, a + x)
    applyInstruction (Up x) (t, d, a) = (t, d, a - x)
