{-# LANGUAGE TypeFamilies #-}

module Game where

type Score = Int

class GameState game where
  type Move game
  chooseBestMove :: game -> [Move game] -> Maybe (Move game) -> (Move game)
  getScore :: game -> (Move game) -> Score
