module AtomicCell where

import BoardCellState
import Cell
import PlayerTurn

{-|
    Атомарная ячейка локального поля.
-}
newtype AtomicCell = AtomicCell BoardCellState

{-|
    Представитель класса типов Cell для типа AtomicCell.
-}
instance Cell AtomicCell where

    -- | Атомарная ячейка содержит только свое состояние - возвращаем его.
    state (AtomicCell cellState) = cellState

    -- | Если ячейка уже занята, возвращаем ошибку. 
    applyTurn _ (AtomicCell (Owned _)) = Left "Cell is owned already!"

    -- | В противном случае присваиваем ячейку игроку.
    applyTurn playerTurn _ = Right $ AtomicCell $ Owned $ player playerTurn