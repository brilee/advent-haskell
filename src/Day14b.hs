module Day14b where
import Data.Map (fromListWith, toList)
import Data.List.Split
import Data.List (sortOn)
import Data.Maybe

type Dimer = (Char, Char)
type GrowthRules = [(Dimer, [Dimer])]
type Polymer = ([(Dimer, Int)], Char, Char)

applyGrowth :: GrowthRules -> Polymer -> Polymer
applyGrowth rules (polymer, start, end) =
    let newDimers = \(dimer, count) -> zip (fromJust $ lookup dimer rules) (repeat count)
        newGrowths = concatMap newDimers polymer
        newPolymer = toList (fromListWith (+) newGrowths)
    in (newPolymer, start, end)

countDimers :: String -> Polymer
countDimers str = 
    let polymer = toList (fromListWith (+) [(x, 1) | x <- zip str (tail str)])
    in (polymer, head str, last str)

countMonomers :: Polymer -> [(Char, Int)]
countMonomers (polymer, start, end) =
    let monomers = concatMap (\((d1, d2), count) -> [(d1, count), (d2, count)]) polymer
        doubleCount = toList (fromListWith (+) (monomers ++ [(start, 1), (end, 1)]))
    in map (\(d, count) -> (d, count `div` 2)) doubleCount

parseGrowthRule :: String -> (Dimer, [Dimer])
parseGrowthRule rawText =
    let dimer:output:_ = splitOn " -> " rawText
        d1:d2:_ = dimer
        o = head output
    in ((d1, d2), [(d1, o), (o, d2)])

parseInput :: IO (Polymer, GrowthRules)
parseInput = do
    start <- getLine
    _ <- getLine
    rawGrowthRules <- lines <$> getContents
    return (countDimers start, map parseGrowthRule rawGrowthRules)

main = do
    (start, rules) <- parseInput
    let growthSequence = iterate (applyGrowth rules) start
        step40 = sortOn snd $ countMonomers $ growthSequence !! 40
        mostFreq = last step40
        leastFreq = head step40
    print $ snd mostFreq - snd leastFreq
