{-# LANGUAGE TypeFamilies #-}

import Game
import Prelude hiding (Either (..))

instance GameState Checkers where
  type Move Checkers = (From, To)

  getScore state move = 5

data Checkers = Checkers
  { board :: Board,
    player :: Color Player
  }

type Position = (Int, Int)

type From = Position

type To = Position

type Board = [[Maybe (Color Piece)]]

data Piece = Man | King
  deriving (Eq, Show)

data Color a = Black a | White a
  deriving (Eq, Show)

data Player

data Direction = Up | Down deriving (Eq)

data Side = Left | Right deriving (Eq)

getOppositeColor :: Color a -> Color a
getOppositeColor (Black a) = (White a)
getOppositeColor (White a) = (Black a)

exampleBoard :: Board
exampleBoard =
  (take 3 $ repeat $ (take 3 $ repeat (Just (White Man))) ++ [Just (White King)])
    ++ (take 2 $ repeat (take 4 $ repeat Nothing))
    ++ (take 3 $ repeat $ (take 3 $ repeat (Just (Black Man))) ++ [Just (Black King)])

initialBoard :: Board
initialBoard =
  [ [Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man)],
    [Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man), Nothing],
    [Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man)],
    take 8 (repeat Nothing),
    take 8 (repeat Nothing),
    [Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man)],
    [Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing],
    [Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man)]
  ]

printPiece :: Maybe (Color Piece) -> String
printPiece Nothing = " "
printPiece (Just (Black Man)) = "o"
printPiece (Just (Black King)) = "♔"
printPiece (Just (White Man)) = "●"
printPiece (Just (White King)) = "♚"

printBoard :: Board -> IO ()
printBoard [] = return ()
printBoard (row : rest) = do
  printRow row
  printBoard rest

flipBoard :: Board -> Board
flipBoard board = reverse (map reverse board)

printRow :: [Maybe (Color Piece)] -> IO ()
printRow [] = do
  putStrLn ""
  return ()
printRow (x : xs) = do
  putStr (printPiece x ++ "-")
  printRow xs

getField :: Board -> Position -> Maybe (Color Piece)
getField [] _ = Nothing
getField board (i, j) = (board !! i) !! j

getMovesPiece :: Checkers -> Position -> Direction -> [Move Checkers]
getMovesPiece board_ pos dir = getLeftMove board_ pos dir ++ getRightMove board_ pos dir

getLeftMove :: Checkers -> Position -> Direction -> [Move Checkers]
getLeftMove checkers (i, j) dir
  | dir == Up && j == 0 = []
  | dir == Down && j == 7 = []
  | dir == Up && board_ !! (i - 1) !! (j - 1) /= Nothing = []
  | dir == Down && board_ !! (i + 1) !! (j + 1) /= Nothing = []
  | dir == Up = [((i, j), (i - 1, j - 1))]
  | dir == Down = [((i, j), (i + 1, j + 1))]
  where
    board_ = board checkers

getRightMove :: Checkers -> Position -> Direction -> [Move Checkers]
getRightMove checkers (i, j) dir
  | dir == Up && j == 0 = []
  | dir == Down && j == 7 = []
  | dir == Up && board_ !! (i - 1) !! (j + 1) /= Nothing = []
  | dir == Down && board_ !! (i + 1) !! (j - 1) /= Nothing = []
  | dir == Up = [((i, j), (i - 1, j + 1))]
  | dir == Down = [((i, j), (i + 1, j - 1))]
  where
    board_ = board checkers

getLeftJump :: Checkers -> Position -> Direction -> [Move Checkers]
getLeftJump checkers (i, j) dir
  | dir == Up && j == 0 = []
  | dir == Down && j == 7 = []
  | dir == Up && board_ !! (i - 1) !! (j - 1) /= Nothing = []
  | dir == Down && board_ !! (i + 1) !! (j + 1) /= Nothing = []
  | dir == Up = [((i, j), (i - 1, j - 1))]
  | dir == Down = [((i, j), (i + 1, j + 1))]
  where
    board_ = board checkers
    player_ = player checkers
    oppositePlayer = getOppositeColor player_

getDiagonalPosition :: Position -> Direction -> Side -> Maybe (Position)
getDiagonalPosition (0, _) Up _ = Nothing
getDiagonalPosition (7, _) Down _ = Nothing
getDiagonalPosition (_, 0) Up Left = Nothing
getDiagonalPosition (_, 7) Up Right = Nothing
getDiagonalPosition (_, 7) Down Left = Nothing
getDiagonalPosition (_, 0) Down Right = Nothing
getDiagonalPosition (i, j) Up Left = Just (i - 1, j - 1)
getDiagonalPosition (i, j) Up Right = Just (i - 1, j + 1)
getDiagonalPosition (i, j) Down Left = Just (i + 1, j + 1)
getDiagonalPosition (i, j) Down Right = Just (i + 1, j - 1)
getDiagonalPosition _ _ _ = Nothing

getRightJump :: Board -> Position -> Direction -> [Move Checkers]
getRightJump board (i, j) dir = []

jumpPos :: Piece -> Position -> Direction -> [Position]
jumpPos Man (0, _) Up = []
jumpPos Man (7, _) Down = []

-- getMoves :: Checkers -> [Move Checkers]

etMoves _ = []