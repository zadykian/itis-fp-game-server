{-# LANGUAGE DeriveGeneric #-}

module Player where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

{-|
    Тип игрока.
    Используется для указания принадлежности ячейки одному из игроков,
    а также для идентификации игрока в составе хода.
-}
data Player = X | O deriving (Eq, Show, Generic)

{-|
    Представитель класса типов ToJSON для типа Player.
-}
instance ToJSON Player

{-|
    Представитель класса типов FromJSON для типа Player.
-}
instance FromJSON Player