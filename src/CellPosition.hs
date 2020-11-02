module CellPosition (CellPosition, toEnum, fromEnum) where

{-|
     Позиция ячейки игрового поля.
     Ограничивается целочисленными значениями в диапазоне [0..8].
     Ячейки нумеруются следующим образом:

         0 | 1 | 2
         --+---+--
         3 | 4 | 5
         --+---+--
         6 | 7 | 8
-}
newtype CellPosition = CellPosition Int deriving (Eq, Ord, Show)

{-|
    Представитель класса типов Bounded для типа CellPosition.
    Используется для задания ограничения допустимых значений.
-}
instance Bounded CellPosition where
    minBound = CellPosition 0
    maxBound = CellPosition 8

{-|
    Представитель класса типов Enum для типа CellPosition.
-}
instance Enum CellPosition where

    toEnum intValue
        | cellToCreate < minBound || cellToCreate > maxBound = error "Cell position must be in range [0..8]"
        | otherwise = cellToCreate
        where cellToCreate = CellPosition intValue

    fromEnum (CellPosition intValue) = intValue
