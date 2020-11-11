{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module PlayerTurn where

import Player
import CellPosition

import Control.Applicative
import Data.Aeson
import GHC.Generics
import Data.Swagger
import Control.Lens hiding ((.=))

{-|
    Ход игрока.
-}
data PlayerTurn = PlayerTurn [CellPosition] Player deriving (Eq, Show, Generic)

instance ToJSON PlayerTurn where
    toJSON (PlayerTurn cellList playerOfTurn) = object
        [
            "GlobalBoardPosition" .= head cellList,
            "LocalBoardPosition" .= cellList !! 1,
            "Player" .= playerOfTurn
        ]

instance FromJSON PlayerTurn where
    parseJSON (Object jsonValue) = do
        globalBoardPosition <- jsonValue .: "GlobalBoardPosition"
        localBoardPosition <- jsonValue .: "LocalBoardPosition"
        playerOfTurn <- jsonValue .: "Player"
        return $ PlayerTurn [globalBoardPosition, localBoardPosition] playerOfTurn
    parseJSON _ = empty

instance ToSchema PlayerTurn where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Player's turn example."
        & mapped.schema.example ?~ toJSON (PlayerTurn (map toEnum [4, 4]) X)

{-|
    Получить глобальную позицию в составе хода.
-}
globalPosition :: PlayerTurn -> CellPosition
globalPosition (PlayerTurn (global : _) _) = global
globalPosition _ = error "Player turn does not contain global position!"

{-|
    Получить локальную позицию в составе хода.
-}
localPosition :: PlayerTurn -> CellPosition
localPosition (PlayerTurn (_ : local : _) _) = local
localPosition _ = error "Player turn does not contain local position!"

{-|
    Получить игрока, выполняющего ход.
-}
player :: PlayerTurn -> Player
player (PlayerTurn [] playerOfTurn) = playerOfTurn
player (PlayerTurn (_ : innerPart) playerOfTurn) = player $ PlayerTurn innerPart playerOfTurn