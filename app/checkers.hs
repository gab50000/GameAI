module Main where

import Checkers

main = aiAgainstAI initialState
  where
    initialState = Checkers initialBoard Black Up