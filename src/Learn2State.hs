module Learn2State where

import Control.Monad (when)
import Control.Monad.State (MonadState (put), StateT (runStateT), gets, modify)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.List (delete, find)

newtype Player = Player
  { pName :: String
  }
  deriving (Eq, Show)

data GameState = GameState
  { gsPlayers :: [Player]
  , gsScores :: HashMap String Int
  }
  deriving (Eq, Show)

-- This is a simple "monad stack". We can combine multiple monads
-- using monad transformers. Here we are combining IO and State (via StateT).
type Game = StateT GameState IO

runGame :: (StateT s IO) a -> s -> IO (a, s)
runGame = runStateT

initialState :: GameState
initialState =
  GameState
    { gsPlayers = []
    , gsScores = HMap.empty
    }

addPlayer :: Player -> Game ()
addPlayer player = do
  modify (addPlayer' player)
  where
    addPlayer' :: Player -> GameState -> GameState
    addPlayer' p s =
      let players = gsPlayers s
          score = gsScores s
       in s
            { gsPlayers = p : players
            , gsScores = HMap.insert (pName p) 0 score
            }

removePlayer :: Player -> Game ()
removePlayer player = do
  players <- gets gsPlayers
  when (player `elem` players) $
    modify
      ( \s ->
          let players = gsPlayers s
              score = gsScores s
           in s
                { gsPlayers = delete player players
                , gsScores = HMap.delete (pName player) score
                }
      )

attack :: Player -> Player -> Game ()
attack source target = do
  damage <- hitDamage source target
  modify
    ( \s ->
        let players = gsPlayers s
            scores = gsScores s
         in s
              { gsScores =
                  HMap.alter
                      (fmap (\x -> x - damage))
                      (pName target)
                    . HMap.alter
                          (fmap (+ damage))
                          (pName source)
                    $ scores
              }
    )

hitDamage :: Player -> Player -> Game Int
hitDamage source target = do
  score <- gets gsScores
  let srcScore = HMap.lookup (pName source) score
      tgtScore = HMap.lookup (pName target) score
   in return $ hitDamage' srcScore tgtScore

hitDamage' :: Maybe Int -> Maybe Int -> Int
hitDamage' (Just srcScore) (Just tgtScore) =
  (tgtScore - srcScore) `div` 2
hitDamage' _ _ = 0

setScore :: Player -> Int -> Game ()
setScore player score =
  modify (setScore' player score)

setScore' :: Player -> Int -> GameState -> GameState
setScore' player score state =
  let name = pName player
   in state{gsScores = HMap.insert name score (gsScores state)}