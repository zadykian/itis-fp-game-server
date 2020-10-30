{-# LANGUAGE FlexibleInstances #-}

module GameBoard(
    GameBoard, getBoardCell,
    LocalBoard, emptyLocalBoard,
    GlobalBoard, emptyGlobalBoard
) where

import AtomicCell
import BoardCell
import BoardCellState
import CellPosition

{-|
    Игровое поле, состоящее из девяти ячеек.
-}
newtype GameBoard cell = GameBoard [cell]

{-|
    Получить ячейку игрового поля по позиции CellPosition.
-}
getBoardCell :: CellPosition -> GameBoard cell -> cell
getBoardCell position (GameBoard cellList) = cellList !! toIntValue position



{-|
    Локальное игровое поле.
-}
type LocalBoard = GameBoard AtomicCell

{-|
    Конструктор пустого локального поля.
-}
emptyLocalBoard :: LocalBoard
emptyLocalBoard = GameBoard $ replicate 9 (AtomicCell Free)

{-|
    Представитель класса типов BoardCell для типа LocalBoard.
-}
instance BoardCell LocalBoard where

    -- todo
    state = undefined
    
    -- todo
    turnCanBeApplied = undefined

    -- todo
    applyTurn = undefined



{-|
    Глобальное игровое поле.
-}
type GlobalBoard = GameBoard LocalBoard

{-|
    Конструктор пустого глобального поля.
-}
emptyGlobalBoard :: GlobalBoard
emptyGlobalBoard = GameBoard $ replicate 9 emptyLocalBoard

{-|
    Представитель класса типов BoardCell для типа GlobalBoard.
-}
instance BoardCell GlobalBoard where

    -- todo
    state = undefined
    
    -- todo
    turnCanBeApplied = undefined

    -- todo
    applyTurn = undefined
