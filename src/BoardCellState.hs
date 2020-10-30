module BoardCellState where

import PlayerMark

{-|
    Состояние ячейки игрового поля.
    Доступные значения: захвачена игроком 'X' или 'O'; свободна.
-}
data BoardCellState = Owned PlayerMark | Free deriving (Eq, Show)