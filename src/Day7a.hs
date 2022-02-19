module Day7a where
import Data.List
import Data.List.Split
import Debug.Trace (traceShowId)

parseCrabs :: IO [Int]
parseCrabs = map read . splitOn "," <$> getLine
main = do
    crabs <- parseCrabs
    let sortedCrabs = sort crabs
        xMedian = sortedCrabs !! (length crabs `div` 2)
        crabDistances = map (\x -> abs (x - xMedian )) crabs
    print $ sum crabDistances

