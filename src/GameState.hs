module GameState (GameState, ErrorMessage, tryApplyTurn) where

import GameBoard(GlobalBoard, getBoardCell)
import PlayerTurn
import BoardCell
import PlayerMark
import BoardCellState

{-|
    Состояние игры.
    Описывается как глобальное поле и последний ход одного из игроков.
-}
data GameState = GameState GlobalBoard (Maybe PlayerTurn)

{-|
    Сообщение об ошибке выполнения операции.
-}
type ErrorMessage = String

{-|
    Попытаться применить ход к состоянию игры.
-}
tryApplyTurn :: PlayerTurn -> GameState -> Either ErrorMessage GameState
tryApplyTurn playerTurn gameState = if turnCanBeAppliedToState playerTurn gameState 
    then Right $ applyTurnToState playerTurn gameState
    else Left "turn can't be applied!"

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
     3. а) Позиция локального поля соответствует позиции ячейки предыдущего хода.
        - или -
        б) Предыдущий ход определил недоступное локальное поле.
-}
turnCanBeAppliedToState currentTurn (GameState globalBoard (Just previousTurn)) =
    player currentTurn /= player previousTurn
    && turnCanBeApplied currentTurn globalBoard
    && (localPosition previousTurn == globalPosition currentTurn || previousTurnDirectedToOwned)
    where
        previousTurnDirectedToOwned = case state localBoardSetByPreviousTurn of
            Owned _ -> True
            Free -> False
        localBoardSetByPreviousTurn = getBoardCell (localPosition previousTurn) globalBoard

{-|
    Применить ход к состоянию игры.
-}
applyTurnToState :: PlayerTurn -> GameState -> GameState
applyTurnToState playerTurn (GameState globalBoard _) =
    GameState (applyTurn playerTurn globalBoard)
    $ Just playerTurn