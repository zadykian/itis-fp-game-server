{-# LANGUAGE OverloadedStrings #-}

module Server where

import Servant
import Servant.Swagger
import Data.Swagger
import Servant.Swagger.UI
import HttpApi

import GameState(GameState, newGameState, tryApplyTurn)
import Data.UUID (UUID)
import PlayerTurn
import Control.Lens

serverApplication :: Application
serverApplication = serve (Proxy :: Proxy HttpApiWithSwagger) httpServerWithSwagger

{-|
    Swagger-спецификация API.
-}
swaggerSpecification :: Swagger
swaggerSpecification = toSwagger (Proxy :: Proxy HttpApi)
    & info.title .~ "Ultimate Tic-Tac-Toe API"
    & info.version .~ "0.3.0.0"
    & info.description ?~ "API for communicating with game server."

{-|
    HTTP-сервер игры.
-}
httpServerWithSwagger :: Server HttpApiWithSwagger
httpServerWithSwagger =
    (createNewGame
    :<|> getGameState
    :<|> applyTurnToGame)
    :<|> swaggerSchemaUIServer swaggerSpecification
    -- :<|> fallbackSwaggerUI
    where
        {-|
            Создать новую игру на сервере.
        -}
        createNewGame :: Handler UUID
        -- todo
        createNewGame = undefined

        {-|
            Получить состояние игры по идентификатору.
        -}
        getGameState :: Maybe UUID -> Handler GameState
        getGameState Nothing = return400error
        -- todo
        getGameState gameUuid = undefined

        {-|
            Применить ход к состоянию игры.
        -}
        applyTurnToGame :: Maybe UUID -> PlayerTurn -> Handler GameState
        applyTurnToGame Nothing _ = return400error
        -- todo
        applyTurnToGame gameUuid playerTurn = undefined

        -- Сформировать ответ при отсутствии заголовка 'Game-Uuid' в запросе.
        return400error = throwError $ err400 { errReasonPhrase = "Game-Uuid header is required!"}