{-# LANGUAGE FlexibleInstances #-}

module GameBoard(
    GameBoard, getBoardCell,
    LocalBoard, emptyLocalBoard,
    GlobalBoard, emptyGlobalBoard
) where

import AtomicCell
import BoardSegment
import BoardSegmentState
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
    Глобальное игровое поле.
-}
type GlobalBoard = GameBoard LocalBoard

{-|
    Конструктор пустого глобального поля.
-}
emptyGlobalBoard :: GlobalBoard
emptyGlobalBoard = GameBoard $ replicate 9 emptyLocalBoard


{-|
    Представитель класса типов BoardSegment для типа [GameBoard cell],
    причем вложенная в GameBoard ячейка также должна являться представителем класса BoardSegment.

    Образуется следующая вложенность типов:
    GlobalBoard --> LocalBoard --> AtomicCell.
-}
instance (BoardSegment cell) => BoardSegment (GameBoard cell) where

        -- todo
        state = undefined

        -- todo
        turnCanBeApplied = undefined

        -- todo
        applyTurn = undefined