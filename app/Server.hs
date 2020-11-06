module Server where

import Servant (Server, (:<|>))
import HttpApi

import GameState(newGameState, tryApplyTurn)

{-|
    HTTP-сервер игры.
-}
httpServer :: Server HttpApi
