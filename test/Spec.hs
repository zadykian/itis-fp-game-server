import Test.HUnit
import GameBoard
import AtomicCell
import BoardSegmentState

main :: IO Counts
main = runTestTT $ TestList [
    TestLabel "empty local board creation" createEmptyLocalBoardTest,
    TestLabel "empty global board creation" createEmptyGlobalBoardTest
    ]


{-|
    Тест создания пустого локального поля.
-}
createEmptyLocalBoardTest :: Test
createEmptyLocalBoardTest = TestCase (
    assertEqual "emptyLocalBoard"
        emptyLocalBoard
        $ GameBoard $ replicate 9 $ AtomicCell Free
    )

{-|
    Тест создания пустого глобального поля.
-}
createEmptyGlobalBoardTest :: Test
createEmptyGlobalBoardTest = TestCase (
    assertEqual "emptyGlobalBoard"
        emptyGlobalBoard
        $ GameBoard $ replicate 9 emptyLocalBoard
    )