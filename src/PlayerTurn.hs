module PlayerTurn where

import Player
import CellPosition

{-|
    Ход игрока.
    Определяется как рекурсивный тип, состоящий из последовательности позиций и типа игрока: 
        [CellPosition | ... | CellPosition | Player].
-}
data PlayerTurn = PlayerTurn [CellPosition] Player

{-
    Получить глобальную позицию в составе хода.
-}
globalPosition :: PlayerTurn -> CellPosition
globalPosition (PlayerTurn (global : _) _) = global
globalPosition _ = error "Player turn does not contain global position!"

{-
    Получить локальную позицию в составе хода.
-}
localPosition :: PlayerTurn -> CellPosition
localPosition (PlayerTurn (_ : local : _) _) = local
localPosition _ = error "Player turn does not contain local position!"

{-
    Получить игрока, выполняющего ход.
-}
player :: PlayerTurn -> Player
player (PlayerTurn [] playerOfTurn) = playerOfTurn
player (PlayerTurn (_ : innerPart) playerOfTurn) = player $ PlayerTurn innerPart playerOfTurn