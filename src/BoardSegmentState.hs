{-# LANGUAGE DeriveGeneric #-}

module BoardSegmentState where

import Player
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

{-|
    Состояние сегмента игрового поля.
    Доступные значения: захвачена игроком 'X' или 'O'; свободна.
-}
data BoardSegmentState = Owned Player | Free deriving (Eq, Show, Generic)

{-|
    Представитель класса типов ToJSON для типа BoardSegmentState.
-}
instance ToJSON BoardSegmentState

{-|
    Представитель класса типов FromJSON для типа BoardSegmentState.
-}
instance FromJSON BoardSegmentState