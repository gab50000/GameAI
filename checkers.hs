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
    [Nothing, Just (White Man), Nothing] ++ take 5 (repeat Nothing),
    [Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing],
    [Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man)],
    [Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing]
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
  putStr (printPiece x ++ "|")
  printRow xs

getField :: Board -> Position -> Maybe (Color Piece)
getField [] _ = Nothing
getField board (i, j) = (board !! i) !! j

getMoves :: Board -> Position -> Direction -> [Move Checkers]
getMoves board_ pos dir = leftMove ++ rightMove
  where
    leftMove = getMove board_ pos dir Left
    rightMove = getMove board_ pos dir Right

getMove :: Board -> Position -> Direction -> Side -> [Move Checkers]
getMove board_ pos dir side
  | Nothing <- diagPos = []
  | Just newPos <- diagPos,
    Nothing <- getPiece board_ newPos =
    [(pos, newPos)]
  | otherwise = []
  where
    (i, j) = pos
    diagPos = getDiagonalPosition (i, j) dir side

getJump :: Board -> Position -> Direction -> Side -> [Move Checkers]
getJump board pos dir side
  | destination /= Nothing = []
  | Just playerColor <- player_,
    oppositePlayer <- getOppositeColor playerColor,
    Just enemyPos <- diag1,
    Just playerOnDiag <- getPiece board enemyPos,
    playerOnDiag == oppositePlayer,
    Just newPos <- diag2 =
    [(pos, newPos)]
  | otherwise = []
  where
    diagonalOnce pos' = getDiagonalPosition pos' dir side
    diagonalTwice pos' = diagonalOnce pos' >>= diagonalOnce
    diag1 = diagonalOnce pos
    diag2 = diagonalTwice pos
    destination = diag2 >>= getPiece board
    player_ = getPiece board pos

getPiece :: Board -> Position -> Maybe (Color Piece)
getPiece board_ (i, j) = board_ !! i !! j

getDiagonalPosition :: Position -> Direction -> Side -> Maybe (Position)
-- White fields are no valid fields
getDiagonalPosition (i, j) _ _
  | mod (i + j) 2 == 0 = Nothing
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
