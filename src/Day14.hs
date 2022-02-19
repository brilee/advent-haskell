module Day14 where
import Data.Map (fromListWith, toList)
import Data.List.Split
import Data.List (sortOn)

type Dimer = (Char, Char)
type GrowthRules = [(Dimer, Char)]

applyGrowth :: GrowthRules -> String -> String
applyGrowth rules [] = []
applyGrowth rules [x] = [x]
applyGrowth rules (x:y:zs) =
    let maybeNew = lookup (x, y) rules
    in case maybeNew of
        Just new -> x:new:applyGrowth rules (y:zs)
        Nothing  -> x:applyGrowth rules (y:zs)

parseGrowthRule :: String -> (Dimer, Char)
parseGrowthRule rawText =
    let dimer:output:_ = splitOn " -> " rawText
        d1:d2:_ = dimer
        o = head output
    in ((d1, d2), o)

parseInput :: IO (String, GrowthRules)
parseInput = do
    start <- getLine
    _ <- getLine
    rawGrowthRules <- lines <$> getContents
    return (start, map parseGrowthRule rawGrowthRules)

main = do
    (start, rules) <- parseInput
    let growthSequence = iterate (applyGrowth rules) start
        countFrequencies str = sortOn fst $ toList (fromListWith (+) [(x, 1) | x <- str])
        step10 = sortOn snd $ countFrequencies $ growthSequence !! 10
        mostFreq = last step10
        leastFreq = head step10
    print $ snd mostFreq - snd leastFreq
    print $ countFrequencies $ growthSequence !! 1
    print $ countFrequencies $ growthSequence !! 2
    print $ countFrequencies $ growthSequence !! 3
    print $ countFrequencies $ growthSequence !! 4
    print $ countFrequencies $ growthSequence !! 5
    print $ countFrequencies $ growthSequence !! 6
    print $ countFrequencies $ growthSequence !! 7
    print $ countFrequencies $ growthSequence !! 8
