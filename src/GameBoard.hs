{-# LANGUAGE FlexibleInstances #-}

module GameBoard where

import AtomicCell
import BoardCell
import BoardCellState

{-|
    Игровое поле, состоящее из девяти ячеек.
-}
newtype GameBoard cell = GameBoard (
    (cell, cell, cell),
    (cell, cell, cell),
    (cell, cell, cell))
    
{-|
    Локальное игровое поле.
-}
type LocalBoard = GameBoard AtomicCell

{-|
    Представитель класса типов BoardCell для типа LocalBoard.
-}
instance BoardCell LocalBoard where

    -- todo
    state _ = Free
    
    -- todo
    turnCanBeApplied _ _ = True

    -- todo
    applyTurn _ localBoard = localBoard

{-|
    Глобальное игровое поле.
-}
type GlobalBoard = GameBoard LocalBoard

{-|
    Представитель класса типов BoardCell для типа GlobalBoard.
-}
instance BoardCell GlobalBoard where

    -- todo
    state _ = Free
    
    -- todo
    turnCanBeApplied _ _ = True

    -- todo
    applyTurn _ globalBoard = globalBoard
