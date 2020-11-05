{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module HttpApi where

import Servant.API (Get, Post, (:>), (:<|>), ReqBody, JSON)
import GameState (GameState)
import PlayerTurn (PlayerTurn)


-- | Декларация HTTP Api, содержащая сигнатуры всех методов.
type HttpApi =
    -- | Получить новое состояние игры.
    "get-new-game-state"
        :> Get '[JSON] GameState

    -- | Применить ход к состоянию игры.
    :<|> "apply_turn_to_game_state"
        :> ReqBody '[JSON] (PlayerTurn, GameState) 
        :> Post '[JSON] GameState