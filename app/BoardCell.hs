module BoardCell where

{-|
    Ячейка игрового поля.
    Доступные значения: захвачена X-ом; захвачена Y-ом; свободна.
-}
data BoardCell = OwnedByX | OwnedByY | Free deriving (Eq, Show)