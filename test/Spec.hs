import Test.HUnit

import GameBoard
import AtomicCell
import BoardSegmentState
import Player
import PlayerTurn
import BoardSegment

main :: IO Counts
main = runTestTT $ TestList $ map (uncurry TestLabel) [
    ("empty local board creation", createEmptyLocalBoardTest),
    ("empty global board creation", createEmptyGlobalBoardTest),
    ("apply turn to empty local board", applyTurnToEmptyLocalBoard),
    ("apply turn to empty global board", applyTurnToEmptyGlobalBoard)
    ]

{-|
    Тест создания пустого локального поля.
-}
createEmptyLocalBoardTest :: Test
createEmptyLocalBoardTest = TestCase (
    assertEqual "emptyLocalBoard"
        (GameBoard $ replicate 9 $ AtomicCell Free)
        emptyLocalBoard
    )

{-|
    Тест создания пустого глобального поля.
-}
createEmptyGlobalBoardTest :: Test
createEmptyGlobalBoardTest = TestCase (
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