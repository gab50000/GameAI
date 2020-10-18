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

exampleBoard :: Board
exampleBoard =
  [ [Just X, Just O, Nothing],
    [Just O, Just O, Just X],
    [Just X, Nothing, Just O]
  ]

type Score = Int

type Move = (Int, Int)

isFull :: Board -> Bool
isFull [] = True
isFull (row : rest) = all (/= Nothing) row && isFull rest

determineBestMove :: State -> (Move, Score)
determineBestMove _ = ((0, 0), 0)

getScore :: State -> Move -> Score
getScore state move
  | determineWinner new_board == Just (player) = 1
  where
    new_board = doMove (board state) player move
    player = current_player state
    other_player = getOpponent player

getOpponent :: Player -> Player
getOpponent X = O
getOpponent O = X

doMove :: Board -> Player -> Move -> Board
doMove board player (i, j) = rows_before ++ [row_beginning ++ [Just player] ++ row_end] ++ rows_after
  where
    (rows_before, row : rows_after) = splitAt i board
    (row_beginning, _ : row_end) = splitAt j row

determineWinner :: Board -> Maybe Player
determineWinner board
  | same_along_rows /= Nothing = same_along_rows
  | same_along_cols /= Nothing = same_along_cols
  | same_along_diags /= Nothing = same_along_diags
  | otherwise = Nothing
  where
    same_along_rows = allEqual board
    same_along_cols = allEqual $ transpose board
    same_along_diags = allEqual $ [diagonal board, diagonal $ transpose board]

allEqual :: Board -> Maybe Player
allEqual ((x : xs) : rest_of_board)
  | all (x ==) xs && x /= Nothing = x
  | rest_of_board == [] = Nothing
  | otherwise = allEqual rest_of_board

transpose :: Board -> Board
transpose ([] : _) = []
transpose board = (map head board) : transpose (map tail board)

diagonal :: Board -> [Maybe Player]
diagonal [] = []
diagonal (row : rest) = head row : diagonal rest

main :: IO ()
main = forever $ do
  print X