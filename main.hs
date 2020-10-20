import System.Exit
import Prelude

data Player = X | O deriving (Eq, Show)

data State = State
  { getBoard :: Board,
    getMoves :: [Move],
    getCurrentPlayer :: Player
  }
  deriving (Show)

type Board = [[Maybe Player]]

type Score = Int

type Move = (Int, Int)

exampleBoard :: Board
exampleBoard =
  [ [Just X, Just O, Nothing],
    [Just O, Just O, Just X],
    [Just X, Nothing, Just O]
  ]

exampleState :: State
exampleState =
  State
    { getBoard = exampleBoard,
      getMoves = getPossibleMoves exampleBoard [] (0, 0),
      getCurrentPlayer = X
    }

isFull :: Board -> Bool
isFull [] = True
isFull (row : rest) = all (/= Nothing) row && isFull rest

determineBestMove :: State -> (Move, Score)
determineBestMove _ = ((0, 0), 0)

chooseBestMove :: State -> [Move] -> Maybe Move -> Move
chooseBestMove state (mv : moves) Nothing = chooseBestMove state moves (Just mv)
chooseBestMove state moves (Just old_move)
  | moves == [] = old_move
  | new_score > old_score = chooseBestMove state moves (Just new_move)
  | otherwise = chooseBestMove state rest_moves (Just old_move)
  where
    new_score = getScore state new_move
    old_score = getScore state old_move
    (new_move : rest_moves) = moves

getScore :: State -> Move -> Score
getScore state move
  | determineWinner board == Just (current_player) = 1
  | determineWinner board == Just (opponent) = -1
  | isGameOver new_board = 0
  | otherwise = - (maximum $ map (getScore new_state) new_moves)
  where
    new_board = updateBoard board current_player move
    current_player = getCurrentPlayer state
    opponent = getOpponent current_player
    board = getBoard state
    new_state = updateState state move
    new_moves = getMoves new_state

getOpponent :: Player -> Player
getOpponent X = O
getOpponent O = X

getPossibleMoves :: Board -> [Move] -> (Int, Int) -> [Move]
getPossibleMoves board moves (i, j)
  | board == [] = moves
  | row == [] = getPossibleMoves rest_of_board moves (i + 1, 0)
  | val == Nothing = getPossibleMoves (rest_of_row : rest_of_board) ((i, j) : moves) (i, j + 1)
  | otherwise = getPossibleMoves (rest_of_row : rest_of_board) moves (i, j + 1)
  where
    val : rest_of_row = row
    row : rest_of_board = board

updateState :: State -> Move -> State
updateState state move =
  State
    { getBoard = new_board,
      getMoves = getPossibleMoves new_board [] (0, 0),
      getCurrentPlayer = getOpponent player
    }
  where
    board = getBoard state
    new_board = updateBoard board player move
    player = getCurrentPlayer state

updateBoard :: Board -> Player -> Move -> Board
updateBoard board player (i, j) = rows_before ++ [row_beginning ++ [Just player] ++ row_end] ++ rows_after
  where
    (rows_before, row : rows_after) = splitAt i board
    (row_beginning, _ : row_end) = splitAt j row

isGameOver :: Board -> Bool
isGameOver board = getPossibleMoves board [] (0, 0) == []

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

empty_board :: Board
empty_board = take 3 $ repeat $ take 3 $ repeat Nothing

start_state :: State
start_state =
  State
    { getBoard = empty_board,
      getMoves = getPossibleMoves empty_board [] (0, 0),
      getCurrentPlayer = X
    }

playAgainstHuman :: State -> IO ()
playAgainstHuman state = do
  printBoard $ getBoard state
  print $ "Your turn"
  move <- getInput moves
  let new_state = updateState state move
  checkState new_state
  print $ "Calculating best move..."
  let best_move = chooseBestMove new_state (getMoves new_state) Nothing
  let ai_state = updateState new_state best_move
  checkState ai_state
  playAgainstHuman ai_state
  where
    moves = getMoves state

checkState :: State -> IO ()
checkState state
  | winner /= Nothing = end
  | isGameOver board = end
  | otherwise = return ()
  where
    board = getBoard state
    winner = determineWinner board
    end = printBoard board >> print "Game Over" >> printWinner winner >> exitSuccess

printWinner :: Maybe Player -> IO ()
printWinner x
  | x == Nothing = return ()
  | otherwise = print $ (drawSymbol x) ++ " wins"

printBoard :: Board -> IO ()
printBoard (row : rest)
  | rest /= [] = print (line_str) >> print (take 5 $ repeat '-') >> printBoard rest
  | otherwise = print (line_str)
  where
    (a : b : c : _) = row
    line_str = (drawSymbol a) ++ " " ++ drawSymbol b ++ " " ++ drawSymbol c

drawSymbol :: Maybe Player -> String
drawSymbol (Just X) = "X"
drawSymbol (Just O) = "O"
drawSymbol Nothing = "-"

getInput :: [Move] -> IO (Int, Int)
getInput moves = do
  input <- getLine
  let num = read input - 1
  let i = div num 3
  let j = mod num 3
  if (notElem (i, j) moves)
    then print "Not a valid input" >> getInput moves
    else return (i, j)

main :: IO ()
main = playAgainstHuman start_state
