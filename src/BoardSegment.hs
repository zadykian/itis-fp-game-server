module BoardSegment where

import BoardSegmentState
import PlayerTurn

{-|
    Класс типов сегмента игровой доски.
-}
class BoardSegment segment where

    -- | Получить текущее состояние ячейки.
    state :: segment -> BoardSegmentState

    -- | Определить, может ли ход игрока быть применён к ячейке.
    turnCanBeApplied :: PlayerTurn -> segment -> Bool

    -- | Применить ход игрока к ячейке.
    applyTurn :: PlayerTurn -> segment -> segment