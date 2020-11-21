{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Checkers where

import Data.Char (isDigit, isSpace, toLower)
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.List.Index (indexed)
import Data.Maybe (isJust, isNothing)
import Data.Sequence hiding (Empty, (:<))
import qualified Data.Sequence as Sq
import Game (GameState (..), Score)
import System.Console.ANSI (clearScreen)
import System.Time.Extra (sleep)
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
  | Prelude.null opponentPieces = 100
  | Prelude.null playerPieces = -100
  | n == 0,
    Just nextBoard <- maybeNextBoard =
    List.length (getPositions nextBoard playerColor Man ++ getPositions nextBoard playerColor King) - List.length (getPositions nextBoard opponentColor Man ++ getPositions nextBoard opponentColor King)
  | n > 0,
    Just nextBoard <- maybeNextBoard =
    - (maximum $ map (getScoreUpToDepth (n -1) (Checkers nextBoard nextPlayer nextDirection) playerColor) (getAllMoves nextBoard nextPlayer nextDirection))
  where
    currentBoard = board state
    currentPlayer = player state
    currentDirection = direction state
    maybeNextBoard = makeMove currentBoard move
    opponentColor = oppositeColor playerColor
    opponentPieces = getPositions currentBoard opponentColor Man ++ getPositions currentBoard opponentColor King
    playerPieces = getPositions currentBoard playerColor Man ++ getPositions currentBoard playerColor King
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

multiply :: Int -> [a] -> [a]
multiply n list
  | n > 0 = list ++ multiply (n -1) list
  | otherwise = []

emptyBoard :: Board
emptyBoard = fromList $ multiply 8 [fromList (multiply 8 [Nothing])]

kingBoard :: Board
kingBoard = b2
  where
    b2 = insertPiece b1 (6, 1) (Just (Piece Black Man))
    b1 = insertPiece emptyBoard (3, 4) (Just (Piece White King))

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

getKingMoves :: Board -> Position -> Direction -> Side -> [To]
getKingMoves board_ pos dir side
  | Just (i, j) <- diagPos,
    Nothing <- board_ `index` i `index` j =
    (i, j) : getKingMoves board_ (i, j) dir side
  | otherwise = []
  where
    diagPos = getDiagonalPosition pos dir side

getKingJumps :: Board -> Position -> Direction -> Side -> [To]
getKingJumps board_ pos dir side
  | Just diagPos <- maybeDiagPos,
    Just nextDiagPos <- getDiagonalPosition diagPos dir side,
    Just _ <- getPiece board_ diagPos,
    Nothing <- getPiece board_ nextDiagPos =
    [nextDiagPos]
  | Just diagPos <- maybeDiagPos,
    Nothing <- getPiece board_ diagPos =
    getKingJumps board_ diagPos dir side
  | otherwise = []
  where
    maybeDiagPos = getDiagonalPosition pos dir side

getAllMoves :: Board -> Color -> Direction -> [Move Checkers]
getAllMoves board playerColor direction
  -- If it is possible to beat another piece, a normal move is not allowed
  | not (Prelude.null allJumps) = allJumps
  | otherwise = allJumps ++ allMoves
  where
    allJumps = manJumps ++ kingJumps
    allMoves = manMoves ++ kingMoves
    manJumps = concatMap getJumpsBoardDirection manPositions
    manMoves = concatMap getMovesBoardDirection manPositions
    kingMoves = [(pos, dest) | dir <- [Up, Down], sd <- [Left, Right], pos <- kingPositions, dest <- getKingMoves board pos dir sd]
    kingJumps = [(pos, dest) | dir <- [Up, Down], sd <- [Left, Right], pos <- kingPositions, dest <- getKingJumps board pos dir sd]
    getMovesBoardDirection pos = getMoves board pos direction
    getJumpsBoardDirection pos = getJumps board pos direction
    manPositions = getPositions board playerColor Man
    kingPositions = getPositions board playerColor King

getPositions :: Board -> Color -> PieceKind -> [Position]
getPositions board color_ pieceKind
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
              | Just (Piece color_ pieceKind) == maybePiece = True
              | otherwise = False

makeMove :: Board -> Move Checkers -> Maybe Board
makeMove board ((i, j), (ii, jj))
  | abs (i - ii) >= 2 = Just $ removePiece boardNewPosMinusOldPos (ii + deltaI, jj + deltaJ)
  | abs (i - ii) == 1 = Just boardNewPosMinusOldPos
  | otherwise = Nothing
  where
    newPiece
      | ii `elem` [0, 7],
        Just (Piece color Man) <- currentPiece =
        Just (Piece color King)
      | otherwise = currentPiece

    currentPiece = board `index` i `index` j
    boardNewPos = insertPiece board (ii, jj) newPiece
    boardNewPosMinusOldPos = removePiece boardNewPos (i, j)
    deltaI = signum (i - ii)
    deltaJ = signum (j - jj)

makeMoveState :: Checkers -> Move Checkers -> Maybe Checkers
makeMoveState state move = newState
  where
    newState = case maybeNewBoard of
      Just newBoard -> Just (Checkers newBoard opponentColor opponentDirection)
      Nothing -> Nothing
    maybeNewBoard = makeMove (board state) move
    opponentColor = oppositeColor (player state)
    opponentDirection = oppositeDirection (direction state)

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

getDiagonalPositions :: Position -> Direction -> Side -> [Position] -> [Position]
getDiagonalPositions pos dir side positions
  | Just dp <- diagPos = getDiagonalPositions dp dir side (positions ++ [dp])
  | otherwise = positions
  where
    diagPos = getDiagonalPosition pos dir side

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

humanAgainstAI :: Checkers -> IO ()
humanAgainstAI state = do
  clearScreen
  printBoard $ board state
  let playerColor = player state
  let playerDirection = direction state
  let aiColor = oppositeColor playerColor
  let aiDirection = Down
  newBoard <- waitForMove playerColor (board state) playerDirection
  let newState = Checkers newBoard aiColor aiDirection
  clearScreen
  printBoard (board newState)
  let finalState = calculateMoveState newState
  case finalState of
    Nothing -> print "Game Over"
    Just fs -> humanAgainstAI fs

aiAgainstAI :: Checkers -> IO ()
aiAgainstAI state = do
  clearScreen
  printBoard $ board state
  let newState = calculateMoveState state
  case newState of
    Nothing -> print "Game Over"
    Just ns -> do
      let newBoard = board ns
      clearScreen
      printBoard newBoard
      sleep 0.3
      let finalState = calculateMoveState ns
      case finalState of
        Nothing -> print "Game Over"
        Just fs -> aiAgainstAI fs

calculateMoveState :: Checkers -> Maybe Checkers
calculateMoveState state = makeMoveState state move
  where
    board_ = board state
    color = player state
    dir = direction state
    validMoves = getAllMoves board_ color dir
    move = chooseBestMove state validMoves Nothing

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