{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module HttpApi where

import Servant.API
import Servant.Swagger.UI (SwaggerSchemaUI)

import GameState (GameState)
import PlayerTurn (PlayerTurn)
import Data.UUID (UUID)
import Data.Proxy (Proxy(..))

{-|
    Декларация HTTP Api, содержащая сигнатуры всех методов.
-}
type HttpApi =
    {-|
        Создать новую игру на сервере.
        Клиенту возвращается UUID созданной игры.
    -}
    "CreateNewGame"
        :> Post '[JSON] UUID

    {-|
        Получить состояние игры по идентификатору.
        В случае отсутствия на сервере игры с переданным UUID клиенту возвращается код 400 BadRequest.
    -}
    :<|> "GetGameState"
        :> Header "Game-Uuid" UUID
        :> Get '[JSON] GameState

    {-|
        Применить ход к состоянию игры.
        В случае передачи некорректного хода клиенту возвращается код 400 BadRequest.
    -}
    :<|> "ApplyTurnToGameState"
        :> Header "Game-Uuid" UUID
        :> ReqBody '[JSON] PlayerTurn
        :> Post '[JSON] GameState

{-|
    API для доступа к SwaggerUI.
-}
type SwaggerApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

{-|
    Общий API приложения (методы для взаимодействия с игрой + swagger).
-}
type HttpApiWithSwagger = HttpApi :<|> SwaggerApi

{-|
    Прокси API сервера.
-}
apiProxy :: Proxy HttpApiWithSwagger
apiProxy = Proxy