module BoardSegmentState where

import Player

{-|
    Состояние сегмента игрового поля.
    Доступные значения: захвачена игроком 'X' или 'O'; свободна.
-}
data BoardSegmentState = Owned Player | Free deriving (Eq, Show)