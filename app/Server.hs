module Server where

import Servant (Server, (:<|>))
import HttpApi

import GameState(newGameState, tryApplyTurn)

-- httpServer :: Server HttpApi