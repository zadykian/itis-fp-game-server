module BoardCellState where

import PlayerMark

{-|
    Состояние ячейки игрового поля.
    В данном контексте под ячейкой понимаются:
        - локальное поле в составе глобального;
        - ячейка локального поля.

    Доступные значения: захвачена X-ом; захвачена O-ом; свободна.
-}
data BoardCellState = Owned PlayerMark | Free
    deriving (Eq, Show)