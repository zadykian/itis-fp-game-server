module GameState where

import GameBoard(GlobalBoard)
import PlayerTurn
import BoardCell
import PlayerMark

{-|
    Состояние игры.
    Описывается как глобальное поле и последний ход одного из игроков.
-}
data GameState = GameState GlobalBoard (Maybe PlayerTurn)

{-|
    Определить, может ли ход быть применен к состоянию игры.
-}
turnCanBeAppliedToState :: PlayerTurn -> GameState -> Bool

{-|
    Считаем, что первым всегда ходит 'X'.
-}
turnCanBeAppliedToState currentTurn (GameState _ Nothing) = player currentTurn == X

{-|
     В общем случае ход может быть применен к состоянию игры, если:
     1. Предыдущий ход выполнял другой игрок.
     2. Текущий ход может быть применен к глобальному полю.
-}
turnCanBeAppliedToState currentTurn (GameState globalBoard (Just previousTurn)) =
    player currentTurn /= player previousTurn
    -- todo
    && turnCanBeApplied currentTurn globalBoard

{-|
    Применить ход к состоянию игры.
-}
applyTurnToState :: PlayerTurn -> GameState -> GameState
applyTurnToState playerTurn (GameState globalBoard _) =
    GameState (applyTurn playerTurn globalBoard)
    $ Just playerTurn