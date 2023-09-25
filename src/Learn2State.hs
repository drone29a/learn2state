module Learn2State where

import Control.Monad.State (StateT, gets, MonadState (put), modify)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap



data Player = Player
    { pName :: String
    , pHealth :: Int
    }

data GameState = GameState
    { gsPlayers :: [Player]
    , gsScore :: HashMap String Int
    }

type Game = StateT GameState IO

initialState :: GameState
initialState =
    GameState
        { gsPlayers = []
        , gsScore = HMap.empty
        }

addPlayer :: Player -> Game ()
addPlayer player = do
    modify (addPlayer' player)
      where
        addPlayer' :: Player -> GameState -> GameState
        addPlayer' p s =
            let players = gsPlayers s
                score = gsScore s
            in
            s{ gsPlayers = p : players
             , gsScore = HMap.insert (pName p) 100 score}

removePlayer :: Player -> Game ()
removePlayer = undefined

attack :: Player -> Player -> Game ()
attack source target = undefined

hitDamage :: Player -> Player -> Game Int
hitDamage source target = undefined