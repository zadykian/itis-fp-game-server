module GameBoard where

import AtomicCell

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
    Глобальное игровое поле.
-}
type GlobalBoard = GameBoard LocalBoard