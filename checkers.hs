{-# LANGUAGE TypeFamilies #-}

import Game

type Position = (Int, Int)

type From = Position

type To = Position

instance GameState Checkers where
  type Move Checkers = (From, To)

data Checkers = Checkers
  { black_men :: [Position],
    black_kings :: [Position],
    white_men :: [Position],
    white_kings :: [Position]
  }