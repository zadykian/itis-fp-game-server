{-# LANGUAGE OverloadedStrings #-}

module Server where

import Servant
import Servant.Swagger
import Data.Swagger
import Servant.Swagger.UI

import HttpApi

import Data.UUID (UUID)
import Control.Lens
import Control.Monad.Trans.Reader
import Control.Concurrent.STM.Map

import GameState(GameState, newGameState, tryApplyTurn)
import PlayerTurn
import Control.Monad.Trans.Class (lift)

{-|
    Хранилище игровых партий.
-}
type GameStorage = Map UUID GameState

type AppM = ReaderT GameStorage Handler

gameStorageToHandler :: GameStorage -> AppM a -> Handler a
gameStorageToHandler = flip runReaderT

{-|
    Серверное приложение, инициализируемое хранилищем игровых партий.
-}
serverApplication :: GameStorage -> Application
serverApplication gameStorage =
    serve apiProxy
    $ hoistServer apiProxy (gameStorageToHandler gameStorage) httpServerWithSwagger

{-|
    Swagger-спецификация API.
-}
swaggerSpecification :: Swagger
swaggerSpecification = toSwagger (Proxy :: Proxy HttpApi)
    & info.title .~ "Ultimate Tic-Tac-Toe API"
    & info.version .~ "0.3.0.0"
    & info.description ?~ "API for communicating with game server."

{-|
    Поднять вычисление из Handler в AppM
-}
swaggerFromHandlerToAppM :: Server SwaggerApi -> ServerT SwaggerApi AppM
swaggerFromHandlerToAppM = hoistServer (Proxy :: Proxy SwaggerApi) lift

{-|
    Сервер Swagger-спецификации.
-}
serverDocServer :: ServerT SwaggerApi AppM
serverDocServer = swaggerFromHandlerToAppM $ swaggerSchemaUIServer swaggerSpecification

{-|
    HTTP-сервер игры.
-}
httpServerWithSwagger :: ServerT HttpApiWithSwagger AppM
httpServerWithSwagger =
    (createNewGame
    :<|> getGameState
    :<|> applyTurnToGame)
    :<|> serverDocServer
    where
        {-|
            Создать новую игру на сервере.
        -}
        createNewGame :: AppM UUID
        -- todo
        createNewGame = undefined

        {-|
            Получить состояние игры по идентификатору.
        -}
        getGameState :: Maybe UUID -> AppM GameState
        getGameState Nothing = return400error
        -- todo
        getGameState gameUuid = undefined

        {-|
            Применить ход к состоянию игры.
        -}
        applyTurnToGame :: Maybe UUID -> PlayerTurn -> AppM GameState
        applyTurnToGame Nothing _ = return400error
        -- todo
        applyTurnToGame gameUuid playerTurn = undefined

        -- Сформировать ответ при отсутствии заголовка 'Game-Uuid' в запросе.
        return400error = throwError $ err400 { errReasonPhrase = "Game-Uuid header is required!"}