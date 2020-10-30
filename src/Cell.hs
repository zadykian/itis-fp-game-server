module Cell where

import BoardCellState
import PlayerTurn

{-|
    Класс типов ячейки.
    В данном контексте под ячейкой понимаются:
        - локальное поле в составе глобального;
        - ячейка локального поля.
-}
class Cell cell where

    -- | Получить текущее состояние ячейки.
    state :: cell -> BoardCellState

    -- | Применить ход игрока к ячейке.
    applyTurn :: PlayerTurn -> cell -> Either String cell