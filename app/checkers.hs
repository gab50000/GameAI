module Main where

import Checkers

main = gameAgainstAI initialState
  where
    initialState = Checkers initialBoard Black Up