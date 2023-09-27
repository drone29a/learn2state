module Main where

import Learn2State

main :: IO ()
main = do
  let playerOne = Player "one"
      playerTwo = Player "two"
      playerThree = Player "three"

  (_, endState) <- runGame
    ( addPlayer playerOne
        >> addPlayer playerTwo
        >> addPlayer playerThree
        >> setScore playerTwo 100
        >> attack playerOne playerTwo
        >> setScore playerOne 200
        >> attack playerOne playerTwo
        >> attack playerThree playerOne
    )
    initialState

  print endState
