{-# LANGUAGE DeriveGeneric #-}

module BoardSegmentState where

import Player

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Swagger (ToSchema, declareNamedSchema, genericDeclareNamedSchemaUnrestricted, defaultSchemaOptions)

{-|
    Состояние сегмента игрового поля.
    Доступные значения: захвачена игроком 'X' или 'O'; свободна.
-}
data BoardSegmentState = Owned Player | Free deriving (Eq, Show, Generic)

instance ToJSON BoardSegmentState
instance FromJSON BoardSegmentState
instance ToSchema BoardSegmentState
    where declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions