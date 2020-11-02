import Test.HUnit

import GameBoard
import AtomicCell
import BoardSegmentState
import Player
import PlayerTurn
import BoardSegment

main :: IO Counts
main = runTestTT $ TestList $ map (uncurry TestLabel) [
    ("empty local board creation", createEmptyLocalBoard),
    ("empty global board creation", createEmptyGlobalBoard),
    ("apply turn to empty local board", applyTurnToEmptyLocalBoard),
    ("apply turn to empty global board", applyTurnToEmptyGlobalBoard),
    ("local board is owned by X", localBoardIsOwned)
    ]

{-|
    Тест создания пустого локального поля.
-}
createEmptyLocalBoard :: Test
createEmptyLocalBoard = TestCase (
    assertEqual "emptyLocalBoard"
        (GameBoard $ replicate 9 $ AtomicCell Free)
        emptyLocalBoard
    )

{-|
    Тест создания пустого глобального поля.
-}
createEmptyGlobalBoard :: Test
createEmptyGlobalBoard = TestCase (
    assertEqual "emptyGlobalBoard"
        (GameBoard $ replicate 9 emptyLocalBoard)
        emptyGlobalBoard
    )

{-|
    Применить ход к пустому локальному полю.
-}
applyTurnToEmptyLocalBoard :: Test
applyTurnToEmptyLocalBoard = TestCase (
    assertEqual "turn to empty local board"
        (GameBoard $ AtomicCell (Owned X) : replicate 8 emptyAtomicCell)
        (applyTurn playerTurn emptyLocalBoard)
    )
    where playerTurn = PlayerTurn [toEnum 0] X

{-|
    Применить ход к пустому глобальному полю.
-}
applyTurnToEmptyGlobalBoard :: Test
applyTurnToEmptyGlobalBoard = TestCase (
    assertEqual "turn to empty global board"
        (GameBoard $ modifiedLocalBoard : replicate 8 emptyLocalBoard)
        (applyTurn playerTurn emptyGlobalBoard)
    )
    where
        playerTurn = PlayerTurn [toEnum 0, toEnum 4] O
        modifiedLocalBoard = GameBoard $
            replicate 4 emptyAtomicCell ++
            [AtomicCell (Owned O)] ++
            replicate 4 emptyAtomicCell

{-|
    Получить состояние локального поля, захваченного игроком X.
-}
localBoardIsOwned :: Test
localBoardIsOwned = TestCase (
    assertEqual "local board is owned"
        (Owned X)
        (state localBoard)
    )
    where 
        localBoard = GameBoard $ map AtomicCell 
            [
                Owned X, Free, Free,
                Free, Owned X, Free,
                Free, Free, Owned X
            ]