module GameBoard where

{-|
    Игровое поле, состоящее из девяти ячеек.
-}
newtype GameBoard cell = GameBoard (
    (cell, cell, cell),
    (cell, cell, cell),
    (cell, cell, cell))