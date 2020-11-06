{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module HttpApi where

import Servant.API
import GameState (GameState)
import PlayerTurn (PlayerTurn)
import Data.UUID (UUID)

-- | Декларация HTTP Api, содержащая сигнатуры всех методов.
type HttpApi =
    {-|
        Создать новую игру на сервере.
        Клиенту возвращается UUID созданной игры.
    -}
    "create-new-game"
        :> Post '[JSON] UUID
    
    {-|
        Получить состояние игры по идентификатору.
        В случае отсутствия на сервере игры с переданным UUID клиенту возвращается код 400 BadRequest.
    -}
    :<|> "get-game-state"
        :> Header "Game-Uuid" UUID
        :> Get '[JSON] GameState
    
    {-|
        Применить ход к состоянию игры.
        В случае передачи некорректного хода клиенту возвращается код 400 BadRequest.
    -}
    :<|> "apply-turn-to-game-state"
        :> Header "Game-Uuid" UUID
        :> ReqBody '[JSON] PlayerTurn 
        :> Post '[JSON] GameState