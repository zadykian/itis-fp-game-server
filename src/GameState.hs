{-# LANGUAGE DeriveGeneric #-}

module GameState where

import GameBoard(GlobalBoard, getBoardCell, emptyGlobalBoard)
import PlayerTurn
import BoardSegment
import Player
import BoardSegmentState
import Data.Aeson
import GHC.Generics (Generic)

{-|
    Состояние игры.
    Описывается как глобальное поле и последний ход одного из игроков.
-}
data GameState = GameState GlobalBoard (Maybe PlayerTurn) deriving (Eq, Show, Generic)

{-|
    Представитель класса типов ToJSON для типа GameState.
-}
instance ToJSON GameState

{-|
    Конструктор новой игровой партии.
-}
newGameState :: GameState
newGameState = GameState emptyGlobalBoard Nothing

{-|
    Сообщение об ошибке выполнения операции.
-}
type ErrorMessage = String

{-|
    Попытаться применить ход к состоянию игры.
-}
tryApplyTurn :: PlayerTurn -> GameState -> Either ErrorMessage GameState
tryApplyTurn playerTurn gameState
    | turnCanBeAppliedToState playerTurn gameState = Right $ applyTurnToState playerTurn gameState
    | otherwise = Left "turn can't be applied!"

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