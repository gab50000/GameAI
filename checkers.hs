{-# LANGUAGE TypeFamilies #-}

import Game

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
  deriving (Show)

data Color a = Black a | White a

data Player

exampleBoard :: Board
exampleBoard =
  (take 2 $ repeat $ (take 3 $ repeat (Just (White Man))) ++ [Just (White King)])
    ++ (take 2 $ repeat (take 4 $ repeat Nothing))
    ++ (take 2 $ repeat $ (take 3 $ repeat (Just (Black Man))) ++ [Just (Black King)])

printPiece :: Maybe (Color Piece) -> String
printPiece Nothing = " "
printPiece (Just (Black Man)) = "o"
printPiece (Just (Black King)) = "♔"
printPiece (Just (White Man)) = "●"
printPiece (Just (White King)) = "♚"

printBoard :: Board -> Int -> IO ()
printBoard [] _ = return ()
printBoard (row : rest) i = do
  printRow row i
  printBoard rest (i + 1)

printRow :: [Maybe (Color Piece)] -> Int -> IO ()
printRow [] _ = do
  putStrLn ""
  return ()
printRow (x : xs) i
  | mod i 2 == 0 = do
    putStr (printPiece x ++ "-")
    printRow xs i
  | otherwise = do
    putStr ("-" ++ printPiece x)
    printRow xs i

getField :: Board -> Position -> Maybe (Color Piece)
getField [] _ = Nothing
getField board (i, j) = (board !! i) !! j

getMovesPiece :: Board -> Position -> [Move Checkers]
getMovesPiece _ _ = []

getMoves :: Checkers -> [Move Checkers]
getMoves _ = []