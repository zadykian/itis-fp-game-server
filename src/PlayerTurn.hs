module PlayerTurn where

import Player
import CellPosition

{-|
    Ход игрока.
    Определяется как:
        - метка игрока (X | O);
        - позиция ячейки на глобальном поле (номер локального поля);
        - позиция ячейки на локальном поле.
-}
data PlayerTurn = PlayerTurn
    {
        player :: Player,
        globalPosition :: CellPosition,
        localPosition :: CellPosition
    }