module PlayerTurn where

import Player
import CellPosition

{-|
    Ход игрока.
    Определяется как рекурсивный тип, состоящий из последовательности позиций и типа игрока: 
        [CellPosition | ... | CellPosition | Player].
-}
data PlayerTurn = PlayerTurn Player | WithPosition CellPosition PlayerTurn

{-
    Получить глобальную позицию в составе хода.
-}
globalPosition :: PlayerTurn -> CellPosition
globalPosition (WithPosition global _) = global
globalPosition _ = error "Player turn does not contain global position!"

{-
    Получить локальную позицию в составе хода.
-}
localPosition :: PlayerTurn -> CellPosition
localPosition (WithPosition _ (WithPosition local _)) = local
localPosition _ = error "Player turn does not contain local position!"

{-
    Получить игрока, выполняющего ход.
-}
player :: PlayerTurn -> Player
player (PlayerTurn playerOfTurn) = playerOfTurn
player (WithPosition _ innerPart) = player innerPart