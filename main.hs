import Control.Monad (forever)
import Prelude

data Player = X | O deriving (Eq, Show)

data State = State
  { board :: Board,
    moves :: [Move],
    current_player :: Player
  }
  deriving (Show)

type Board = [[Maybe Player]]

type Score = Int

type Move = (Int, Int)

isFull :: Board -> Bool
isFull [] = True
isFull (row : rest) = all (/= Nothing) row && isFull rest

determineBestMove :: State -> (Move, Score)
determineBestMove _ = ((0, 0), 0)

-- fillBoard :: Board -> (Int, Int) -> Maybe Board
-- fillBoard board _
--     | isFull board = Nothing
--     | otherwise Just board

main :: IO ()
main = forever $ do
  print X