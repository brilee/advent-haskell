module Day6 where
import Data.List
import Data.List.Split
import Debug.Trace (traceShowId)

type Population = [Int] -- Length 9 corresponding to maximum timer of fish.

evolvePopulation :: Population -> Population
evolvePopulation [] = []
evolvePopulation (mature:remaining) =
    take 6 remaining ++ [(remaining !! 6) + mature] ++ [remaining !! 7, mature]

parsePopulation :: IO Population
parsePopulation = do
    rawPopulation <- map read . splitOn "," <$> getLine
    let count x = length . filter (==x)
        popSummary = map (($ rawPopulation) . count) [0..8]
    return $ traceShowId popSummary

main = do
--    let pop = [0, 1, 1, 2, 1, 0, 0, 0, 0]
    pop <- parsePopulation
    let finalPop = iterate evolvePopulation pop !! 256
    print $ sum finalPop
