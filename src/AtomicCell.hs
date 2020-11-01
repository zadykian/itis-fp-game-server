module AtomicCell where

import BoardSegmentState
import BoardSegment
import PlayerTurn

{-|
    Атомарная ячейка локального поля.
-}
newtype AtomicCell = AtomicCell BoardSegmentState

{-|
    Представитель класса типов BoardSegment для типа AtomicCell.
-}
instance BoardSegment AtomicCell where

    -- | Атомарная ячейка содержит только свое состояние - возвращаем его.
    state (AtomicCell cellState) = cellState

    -- | Если ячейка свободна, она может быть присвоена игроку.
    turnCanBeApplied _ (AtomicCell Free) = True
    turnCanBeApplied _ (AtomicCell _) = False

    -- | Присваиваем ячейку игроку.
    applyTurn playerTurn _ = AtomicCell $ Owned $ player playerTurn