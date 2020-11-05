{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module HttpApi where

import Servant.API
import GameState (GameState)

{-|
    Декларация HTTP Api, содержащая сигнатуры всех методов.
-}
type HttpApi =
    -- | Получение нового состояния игры.
    "get_new_game_state" :> Get '[JSON] GameState