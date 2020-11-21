module Main where

import Checkers

main = humanAgainstAI initialState
  where
    initialState = Checkers initialBoard Black Up