{-# LANGUAGE TypeFamilies #-}

module Game where

type Score = Int

class GameState game where
  type Move game

  chooseBestMove :: game -> [Move game] -> Maybe (Move game) -> (Move game)
  chooseBestMove state (mv : moves) Nothing = chooseBestMove state moves (Just mv)
  chooseBestMove state moves (Just old_move)
    | [] <- moves = old_move
    | new_score > old_score = chooseBestMove state moves (Just new_move)
    | otherwise = chooseBestMove state rest_moves (Just old_move)
    where
      new_score = getScore state new_move
      old_score = getScore state old_move
      (new_move : rest_moves) = moves

  getScore :: game -> (Move game) -> Score
