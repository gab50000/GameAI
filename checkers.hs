{-# LANGUAGE TypeFamilies #-}

import Game

type Position = (Int, Int)

type From = Position

type To = Position

type Board = [[Maybe Player]]

data Player = BlackMan | WhiteMan | BlackKing | WhiteKing

exampleBoard :: Board
exampleBoard =
  (take 2 $ repeat $ (take 3 $ repeat (Just WhiteMan)) ++ [Just WhiteKing])
    ++ (take 2 $ repeat (take 4 $ repeat Nothing))
    ++ (take 2 $ repeat $ (take 3 $ repeat (Just BlackMan)) ++ [Just BlackKing])

printPlayer :: Maybe Player -> String
printPlayer Nothing = " "
printPlayer (Just BlackMan) = "o"
printPlayer (Just BlackKing) = "♔"
printPlayer (Just WhiteMan) = "●"
printPlayer (Just WhiteKing) = "♚"

instance GameState Checkers where
  type Move Checkers = (From, To)

  getScore state move = 5

data Checkers = Checkers
  { board :: Board,
    player :: Player
  }

printBoard :: Board -> Int -> IO ()
printBoard [] _ = return ()
printBoard (row : rest) i = do
  printRow row i
  printBoard rest (i + 1)

printRow :: [Maybe Player] -> Int -> IO ()
printRow [] _ = do
  putStrLn ""
  return ()
printRow (x : xs) i
  | mod i 2 == 0 = do
    putStr (printPlayer x ++ "-")
    printRow xs i
  | otherwise = do
    putStr ("-" ++ printPlayer x)
    printRow xs i
