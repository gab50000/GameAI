{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Maybe (isJust, isNothing)
import Game
import System.Exit
import Prelude

instance GameState TicTacToeState where
  type Move TicTacToeState = (Int, Int)

  getScore state move
    | determineWinner new_board == Just current_player = 1
    | determineWinner new_board == Just opponent = -1
    | isGameOver new_board = 0
    | otherwise = - (maximum $ map (getScore new_state) new_moves)
   where
    new_board = updateBoard current_board current_player move
    current_player = player state
    opponent = getOpponent current_player
    current_board = board state
    new_state = updateState state move
    new_moves = getMoves new_state

data TicTacToeState = TicTacToeState
  { board :: Board
  , player :: Player
  }
  deriving (Show)

type Board = [[Maybe Player]]

data Player = X | O deriving (Eq, Show)

exampleBoard :: Board
exampleBoard =
  [ [Just X, Just O, Nothing]
  , [Just O, Just O, Just X]
  , [Just X, Nothing, Just O]
  ]

exampleState :: TicTacToeState
exampleState =
  TicTacToeState
    { board = exampleBoard
    , player = X
    }

isFull :: Board -> Bool
isFull [] = True
isFull (row : rest) = all (/= Nothing) row && isFull rest

getOpponent :: Player -> Player
getOpponent X = O
getOpponent O = X

getMoves :: TicTacToeState -> [Move TicTacToeState]
getMoves state = getPossibleMoves brd [] (0, 0)
 where
  brd = board state

getPossibleMoves :: Board -> [Move TicTacToeState] -> (Int, Int) -> [Move TicTacToeState]
getPossibleMoves board moves (i, j)
  | null board = moves
  | null row = getPossibleMoves rest_of_board moves (i + 1, 0)
  | isNothing val = getPossibleMoves (rest_of_row : rest_of_board) ((i, j) : moves) (i, j + 1)
  | otherwise = getPossibleMoves (rest_of_row : rest_of_board) moves (i, j + 1)
 where
  val : rest_of_row = row
  row : rest_of_board = board

updateState :: TicTacToeState -> Move TicTacToeState -> TicTacToeState
updateState state move =
  TicTacToeState
    { board = new_board
    , player = getOpponent current_player
    }
 where
  current_board = board state
  new_board = updateBoard current_board current_player move
  current_player = player state

updateBoard :: Board -> Player -> Move TicTacToeState -> Board
updateBoard board player (i, j) = rows_before ++ [row_beginning ++ [Just player] ++ row_end] ++ rows_after
 where
  (rows_before, row : rows_after) = splitAt i board
  (row_beginning, _ : row_end) = splitAt j row

isGameOver :: Board -> Bool
isGameOver board = null (getPossibleMoves board [] (0, 0))

determineWinner :: Board -> Maybe Player
determineWinner board
  | isJust same_along_rows = same_along_rows
  | isJust same_along_cols = same_along_cols
  | isJust same_along_diags = same_along_diags
  | otherwise = Nothing
 where
  same_along_rows = allEqual board
  same_along_cols = allEqual $ transpose board
  same_along_diags = allEqual $ [diagonal board, diagonal $ map reverse board]

allEqual :: Board -> Maybe Player
allEqual ((x : xs) : rest_of_board)
  | all (x ==) xs && isJust x = x
  | null rest_of_board = Nothing
  | otherwise = allEqual rest_of_board
allEqual _ = Nothing

transpose :: Board -> Board
transpose ([] : _) = []
transpose board = map head board : transpose (map tail board)

diagonal :: Board -> [Maybe Player]
diagonal [] = []
diagonal (row : rest) = head row : diagonal (map tail rest)

emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 Nothing

startState :: TicTacToeState
startState =
  TicTacToeState
    { board = emptyBoard
    , player = X
    }

humanMove :: TicTacToeState -> IO TicTacToeState
humanMove state = do
  move <- getInput (getMoves state)
  let new_state = updateState state move
  return new_state

computerMove :: TicTacToeState -> TicTacToeState
computerMove state =
  let best_move = chooseBestMove state (getMoves state) Nothing
   in case best_move of
        Nothing -> state
        Just mv -> updateState state mv

playAgainstHuman :: TicTacToeState -> IO ()
playAgainstHuman state = do
  let ai_state = computerMove state
  printBoard $ board ai_state
  checkState ai_state
  putStrLn "Your turn"
  new_state <- humanMove ai_state
  printBoard $ board state
  checkState new_state
  putStrLn "Computers turn"
  playAgainstHuman new_state

checkState :: TicTacToeState -> IO ()
checkState state
  | isJust winner = end
  | isGameOver current_board = end
  | otherwise = return ()
 where
  current_board = board state
  winner = determineWinner current_board
  end = printBoard current_board >> print "Game Over" >> printWinner winner >> exitSuccess

printWinner :: Maybe Player -> IO ()
printWinner x
  | isNothing x = return ()
  | otherwise = print $ drawSymbol x ++ " wins"

rowDelim = "*---*---*---*"

printBoard :: Board -> IO ()
printBoard [] = putStrLn rowDelim
printBoard (row : rest) = do
  putStrLn rowDelim
  putStrLn line_str
  printBoard rest
 where
  (a : b : c : _) = row
  line_str = "| " ++ drawSymbol a ++ " | " ++ drawSymbol b ++ " | " ++ drawSymbol c ++ " |"

drawSymbol :: Maybe Player -> String
drawSymbol (Just X) = "X"
drawSymbol (Just O) = "O"
drawSymbol Nothing = "-"

getInput :: [Move TicTacToeState] -> IO (Int, Int)
getInput moves = do
  input <- getLine
  let num = read input - 1
  let i = div num 3
  let j = mod num 3
  if (i, j) `notElem` moves
    then print "Not a valid input" >> getInput moves
    else return (i, j)

main :: IO ()
main = playAgainstHuman startState
