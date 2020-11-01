{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Sequence hiding (Empty, (:<))
import qualified Data.Sequence as Sq
import Game
import Prelude hiding (Either (..), replicate, reverse, take)
import qualified Prelude as List

pattern Empty <- (Sq.viewl -> Sq.EmptyL) where Empty = Sq.empty

pattern x :< xs <- (Sq.viewl -> x Sq.:< xs) where (:<) = (Sq.<|)

pattern xs :> x <- (Sq.viewr -> xs Sq.:> x) where (:>) = (Sq.|>)

instance GameState Checkers where
  type Move Checkers = (From, To)

  getScore state move = 5
    where
      currentPlayer = player state

-- newState = makeMove state move

data Checkers = Checkers
  { board :: Board,
    player :: Color Player
  }

type Position = (Int, Int)

type From = Position

type To = Position

type Board = Seq (Seq (Maybe (Color Piece)))

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
  fromList $
    map
      fromList
      ( (List.take 3 $ List.repeat $ (List.take 3 $ List.repeat (Just (White Man))) ++ [Just (White King)])
          ++ (List.take 2 $ List.repeat (List.take 4 $ List.repeat Nothing))
          ++ (List.take 3 $ List.repeat $ (List.take 3 $ List.repeat (Just (Black Man))) ++ [Just (Black King)])
      )

initialBoard :: Board
initialBoard =
  fromList $
    map
      fromList
      ( [ [Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man)],
          [Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man), Nothing],
          [Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man), Nothing, Just (White Man)],
          List.replicate 8 Nothing,
          [Nothing, Just (White Man), Nothing] ++ List.replicate 5 Nothing,
          [Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing],
          [Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man)],
          [Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing, Just (Black Man), Nothing]
        ]
      )

printPiece :: Maybe (Color Piece) -> String
printPiece Nothing = " "
printPiece (Just (Black Man)) = "o"
printPiece (Just (Black King)) = "♔"
printPiece (Just (White Man)) = "●"
printPiece (Just (White King)) = "♚"

printBoard :: Board -> IO ()
printBoard Empty = return ()
printBoard (row :< rest) = do
  printRow row
  printBoard rest

flipBoard :: Board -> Board
flipBoard board = reverse (fmap reverse board)

printRow :: Seq (Maybe (Color Piece)) -> IO ()
printRow Empty = do
  putStrLn ""
  return ()
printRow (x :< xs) = do
  putStr (printPiece x ++ "|")
  printRow xs

getField :: Board -> Position -> Maybe (Color Piece)
getField Empty _ = Nothing
getField board (i, j) = (board `index` i) `index` j

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

-- makeMove :: Checkers -> Move Checkers -> Checkers
-- makeMove state ((i, j), (ii, jj))
--   | abs (i - ii) == 2 = removeAt board ((i + ii) / 2, (j + jj) / 2)

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
getPiece board_ (i, j) = board_ `index` i `index` j

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

removePiece :: Board -> Position -> Board
removePiece board pos = insertPiece board pos Nothing

insertPiece :: Board -> Position -> Maybe (Color Piece) -> Board
insertPiece board (i, j) newPiece = update i newRow board
  where
    newRow = update j newPiece oldRow
    oldRow = index board i