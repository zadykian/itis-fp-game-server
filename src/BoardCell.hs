module BoardCell where

import BoardCellState
import PlayerTurn

{-|
    Класс типов ячейки.
-}
class BoardCell cell where

    -- | Получить текущее состояние ячейки.
    state :: cell -> BoardCellState

    -- | Определить, может ли ход игрока быть применён к ячейке.
    turnCanBeApplied :: PlayerTurn -> cell -> Bool

    -- | Применить ход игрока к ячейке.
    applyTurn :: PlayerTurn -> cell -> cell