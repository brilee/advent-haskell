module Day4b where
import Debug.Trace
import Data.List
import Data.List.Split
import Control.Monad
data BingoBoard = BingoBoard [Int] [Bool] deriving Show -- Use a flattened list of len 25.
type NumberSequence = [Int]


makeBingoBoard :: [Int] -> [Bool] -> BingoBoard
makeBingoBoard nums marks
    | length nums /= 25 || length marks /= 25 = 
        error ("received bad bingo length" ++ show nums ++ show marks)
    | otherwise = BingoBoard nums marks

wins = [[i,i+5..24] | i <- [0..4]] ++ [[5*i,5*i+1..5*i+4] | i <- [0..4]]

calculateBoardScore :: Int -> BingoBoard -> Int
calculateBoardScore calledNumber (BingoBoard nums marks)  = 
    case didBoardWin (BingoBoard nums marks) of
        True -> calledNumber * unmarkedScore
            where unmarkedScore = sum [num | (num, mark) <- zip nums marks, not mark]
        False -> 0


didBoardWin :: BingoBoard -> Bool
didBoardWin (BingoBoard nums marks) = any (all (marks !!)) wins

markComplete :: Int -> BingoBoard -> BingoBoard
markComplete num (BingoBoard nums marks) =
    let foundIndex = elemIndex num nums
        updated_marks = case foundIndex of
            Just index ->
                let (before, after) = splitAt index marks
                in before ++ [True] ++ tail after
            Nothing -> marks
    in makeBingoBoard nums updated_marks

applyNumberToBoards :: [BingoBoard] -> Int -> [BingoBoard]
applyNumberToBoards boards num = map (markComplete num) boards

generateCompletedBoards :: [BingoBoard] -> NumberSequence -> [(BingoBoard, Int)]
generateCompletedBoards boardset [] = []
generateCompletedBoards boardset (num:rest) = 
    let playedBoardset = applyNumberToBoards boardset num
        (finishedBoards, unfinishedBoards) = partition didBoardWin playedBoardset
        finishedPairs = zip finishedBoards (repeat num)
    in finishedPairs ++ generateCompletedBoards unfinishedBoards rest

parseNumberStream :: IO [Int]
parseNumberStream = map read . splitOn "," <$> getLine

parseBingoBoard :: IO BingoBoard
parseBingoBoard = do
    nums <- map read . words . unlines <$> replicateM 6 getLine
    return $ makeBingoBoard nums (replicate 25 False)

parseInput :: IO (NumberSequence, [BingoBoard])
parseInput = do
    numbers <- parseNumberStream
    bingoBoards <- replicateM 100 parseBingoBoard
    return (numbers, bingoBoards)

main = do
    (numbers, bingoBoards) <- parseInput
    let completedBoards = generateCompletedBoards bingoBoards numbers
        (firstWinningBoard, winningNumber) = head completedBoards
        (lastWinningBoard, lastWinningNumber) = last completedBoards
        winningScore = calculateBoardScore winningNumber firstWinningBoard
        lastWinningScore = calculateBoardScore lastWinningNumber lastWinningBoard
    print (winningScore, lastWinningScore)
