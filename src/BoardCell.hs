module BoardCell where

import PlayerMark

{-|
    Ячейка игрового поля.
    Доступные значения: захвачена X-ом; захвачена O-ом; свободна.
-}
data BoardCell = Owned PlayerMark | Free 
    deriving (Eq, Show)