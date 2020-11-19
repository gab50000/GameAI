{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Checkers where

import Data.Char (isDigit, isSpace, toLower)
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.List.Index (indexed)
import Data.Maybe (isJust)
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

  getScore state move = getScoreUpToDepth 4 state playerColor move
    where
      playerColor = player state

getScoreUpToDepth :: Int -> Checkers -> Color -> Move Checkers -> Score
getScoreUpToDepth n state playerColor move
  | n == 0,
    Just nextBoard <- maybeNextBoard =
    List.length (getPositions nextBoard playerColor) - List.length (getPositions nextBoard opponentColor)
  | n > 0,
    Just nextBoard <- maybeNextBoard =
    - (maximum $ map (getScoreUpToDepth (n -1) (Checkers nextBoard nextPlayer nextDirection) playerColor) (getAllMoves nextBoard nextPlayer nextDirection))
  where
    currentBoard = board state
    currentPlayer = player state
    currentDirection = direction state
    maybeNextBoard = makeMove currentBoard move
    opponentColor = oppositeColor playerColor
    nextPlayer = oppositeColor currentPlayer
    nextDirection = oppositeDirection currentDirection

data Checkers = Checkers
  { board :: Board,
    player :: Color,
    direction :: Direction
  }

type Position = (Int, Int)

type From = Position

type To = Position

type Board = Seq (Seq (Maybe Piece))

data PieceKind = Man | King deriving (Eq, Show)

data Piece = Piece
  { color :: Color,
    kind :: PieceKind
  }
  deriving (Eq, Show)

data Color = Black | White
  deriving (Eq, Show)

data Direction = Up | Down deriving (Eq)

data Side = Left | Right deriving (Eq)

oppositeColor :: Color -> Color
oppositeColor Black = White
oppositeColor White = Black

oppositeDirection :: Direction -> Direction
oppositeDirection Up = Down
oppositeDirection Down = Up

exampleBoard :: Board
exampleBoard =
  fromList $
    map
      fromList
      ( (List.take 3 $ List.repeat $ (List.take 3 $ List.repeat (Just (Piece White Man))) ++ [Just (Piece White King)])
          ++ (List.take 2 $ List.repeat (List.take 4 $ List.repeat Nothing))
          ++ (List.take 3 $ List.repeat $ (List.take 3 $ List.repeat (Just (Piece Black Man))) ++ [Just (Piece Black King)])
      )

initialBoard :: Board
initialBoard =
  fromList $
    map
      fromList
      [ multiply 4 [Nothing, Just (Piece White Man)],
        multiply 4 [Just (Piece White Man), Nothing],
        multiply 4 [Nothing, Just (Piece White Man)],
        List.replicate 8 Nothing,
        List.replicate 8 Nothing,
        multiply 4 [Just (Piece Black Man), Nothing],
        multiply 4 [Nothing, Just (Piece Black Man)],
        multiply 4 [Just (Piece Black Man), Nothing]
      ]
  where
    multiply :: Int -> [a] -> [a]
    multiply n list
      | n > 0 = list ++ multiply (n -1) list
      | otherwise = []

printPiece :: Maybe Piece -> String
printPiece Nothing = " "
printPiece (Just (Piece Black Man)) = "o"
printPiece (Just (Piece Black King)) = "♔"
printPiece (Just (Piece White Man)) = "●"
printPiece (Just (Piece White King)) = "♚"

printBoard :: Board -> IO ()
printBoard Empty = return ()
printBoard board = do
  printNumbers
  printBoardRecursively indexedBoard
  printNumbers
  where
    letters = fromList ['A' .. 'Z']
    printBoardRecursively :: [(Int, Seq (Maybe Piece))] -> IO ()
    printBoardRecursively [] = return ()
    printBoardRecursively ((i, row) : rest) = do
      let letter = index letters i
      putStr [letter, ' ', ' ']
      printRow row
      putStr [' ', letter]
      putStrLn ""
      printBoardRecursively rest

    indexedBoard = indexed $ toList board
    printNumbers = putStrLn $ "  " ++ concatMap (\num -> " " ++ show num) [1 .. 8]

flipBoard :: Board -> Board
flipBoard board = reverse (fmap reverse board)

printRow :: Seq (Maybe Piece) -> IO ()
printRow Empty = do
  return ()
printRow (x :< xs) = do
  putStr (printPiece x ++ "|")
  printRow xs

getField :: Board -> Position -> Maybe Piece
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

getAllMoves :: Board -> Color -> Direction -> [Move Checkers]
getAllMoves board playerColor direction = concatMap getMovesBoardDirection playerPositions ++ concatMap getJumpsBoardDirection playerPositions
  where
    getMovesBoardDirection pos = getMoves board pos direction
    getJumpsBoardDirection pos = getJumps board pos direction
    playerPositions = getPositions board playerColor

getPositions :: Board -> Color -> [Position]
getPositions board color_
  | board == Empty = []
  | otherwise = toList $ concat $ mapWithIndex getColIndices board
  where
    getColIndices :: Int -> Seq (Maybe Piece) -> [Position]
    getColIndices i row = [(i, j) | j <- colIndices]
      where
        colIndices = findIndicesL hasColor row
          where
            hasColor :: Maybe Piece -> Bool
            hasColor maybePiece
              | Just piece <- maybePiece,
                color piece == color_ =
                True
              | otherwise = False

makeMove :: Board -> Move Checkers -> Maybe Board
makeMove board ((i, j), (ii, jj))
  | abs (i - ii) == 2 = Just $ removePiece boardNewPosMinusOldPos ((i + ii) `div` 2, (j + jj) `div` 2)
  | abs (i - ii) == 1 = Just boardNewPosMinusOldPos
  | otherwise = Nothing
  where
    currentPiece = board `index` i `index` j
    boardNewPos = insertPiece board (ii, jj) currentPiece
    boardNewPosMinusOldPos = removePiece boardNewPos (i, j)

getJumps :: Board -> Position -> Direction -> [Move Checkers]
getJumps board_ pos dir =
  getJump board_ pos dir Left ++ getJump board_ pos dir Right

getJump :: Board -> Position -> Direction -> Side -> [Move Checkers]
getJump board pos dir side
  | isJust destination = []
  | Just player_ <- maybePlayer_,
    playerColor <- color player_,
    oppositePlayer <- oppositeColor playerColor,
    Just enemyPos <- diag1,
    Just playerOnDiag <- getPiece board enemyPos,
    color playerOnDiag == oppositePlayer,
    Just newPos <- diag2 =
    [(pos, newPos)]
  | otherwise = []
  where
    diagonalOnce pos' = getDiagonalPosition pos' dir side
    diagonalTwice pos' = diagonalOnce pos' >>= diagonalOnce
    diag1 = diagonalOnce pos
    diag2 = diagonalTwice pos
    destination = diag2 >>= getPiece board
    maybePlayer_ = getPiece board pos

getPiece :: Board -> Position -> Maybe Piece
getPiece board_ (i, j) = board_ `index` i `index` j

getDiagonalPosition :: Position -> Direction -> Side -> Maybe Position
-- White fields are no valid fields
getDiagonalPosition (i, j) _ _
  | even (i + j) = Nothing
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

removePiece :: Board -> Position -> Board
removePiece board pos = insertPiece board pos Nothing

insertPiece :: Board -> Position -> Maybe Piece -> Board
insertPiece board (i, j) newPiece = update i newRow board
  where
    newRow = update j newPiece oldRow
    oldRow = index board i

parseMove :: String -> Maybe (Move Checkers)
parseMove input
  | (c1 : d1 : c2 : d2 : rest) <- strippedInput,
    [] <- rest,
    isValidLetter c1 && isValidLetter c2,
    isDigit d1 && isDigit d2 =
    convert input
  | otherwise = Nothing
  where
    strippedInput = List.filter (not . isSpace) input
    isValidLetter :: Char -> Bool
    isValidLetter char = toLower char `elem` letters
    isValidNumber :: Int -> Bool
    isValidNumber num = num `elem` [1 .. 8]
    letters = ['a' .. 'h']
    convert :: String -> Maybe (Move Checkers)
    convert (c1 : d1 : c2 : d2 : _)
      | (Just i2, Just ii2) <- (i, ii),
        isValidNumber j && isValidNumber jj =
        Just ((i2, j - 1), (ii2, jj - 1))
      | otherwise = Nothing
      where
        i = elemIndex c1 letters
        j = read [d1] :: Int
        ii = elemIndex c2 letters
        jj = read [d2] :: Int

gameAgainstAI :: Checkers -> IO ()
gameAgainstAI state = do
  printBoard $ board state
  let playerColor = player state
  let playerDirection = direction state
  let aiColor = oppositeColor playerColor
  let aiDirection = Down
  newBoard <- waitForMove playerColor (board state) playerDirection

  printBoard newBoard
  let newState = Checkers {board = newBoard, player = oppositeColor playerColor, direction = oppositeDirection playerDirection}

  let validMoves = getAllMoves newBoard aiColor aiDirection
  let aiMove = chooseBestMove newState validMoves Nothing
  let maybeFinalBoard = makeMove newBoard aiMove
  case maybeFinalBoard of
    Nothing -> print "Game Over"
    Just finalBoard -> do
      let finalState = Checkers finalBoard playerColor playerDirection
      gameAgainstAI finalState

waitForMove :: Color -> Board -> Direction -> IO Board
waitForMove playerColor board dir = do
  move <- parsedMove
  let possibleMoves = getAllMoves board playerColor dir
  if move `elem` possibleMoves
    then do
      let maybeBoard = makeMove board move
      case maybeBoard of
        Nothing -> print "Invalid move!" >> waitForMove playerColor board dir
        Just newBoard -> return newBoard
    else print "Invalid move!" >> waitForMove playerColor board dir
  where
    parsedMove :: IO (Move Checkers)
    parsedMove = do
      playerInput <- getLine
      let maybeMove = parseMove playerInput
      case maybeMove of
        Just mv -> return mv
        Nothing -> print "Invalid move!" >> parsedMove