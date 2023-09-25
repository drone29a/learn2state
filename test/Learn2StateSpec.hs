module Learn2StateSpec where

import Test.Hspec

import Data.HashMap.Strict qualified as HMap
import Learn2State

spec :: Spec
spec = do
  describe "add and remove players" $ do
    it "should add a player" $ do
      ( _
        , GameState
            { gsPlayers = players
            , gsScores = scores
            }
        ) <-
        runGame
            (addPlayer (Player "one"))
            initialState
      players `shouldBe` [Player "one"]
      HMap.toList scores `shouldBe` [("one", 0)]

    it "should remove a player" $ do
      ( _
        , GameState
            { gsPlayers = players
            , gsScores = scores
            }
        ) <-
        runGame
          (addPlayer (Player "one") >> removePlayer (Player "one"))
          initialState
      players `shouldBe` []
      HMap.toList scores `shouldBe` []

  describe "player attacks" $ do
    it "should do no damage when players with same score battle" $ do
      let playerOne = Player "one"
          playerTwo = Player "two"
      ( _
        , GameState
            { gsPlayers = _players
            , gsScores = scores}
        ) <-
        runGame
          (addPlayer playerOne >>
           addPlayer playerTwo >>
           attack playerOne playerTwo)
          initialState
      HMap.toList scores `shouldContain` [("one", 0), ("two", 0)]
    
    it "should do damage when attacker has lower score than defender" $ do
      let playerOne = Player "one"
          playerTwo = Player "two"
      ( _
        , GameState
            { gsPlayers = _players
            , gsScores = scores}
        ) <-
        runGame
          (addPlayer playerOne >>
           addPlayer playerTwo >>
           setScore playerTwo 100 >>
           attack playerOne playerTwo)
          initialState
      HMap.toList scores `shouldContain` [("one", 50), ("two", 50)]

