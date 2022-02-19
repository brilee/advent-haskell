module Day8 where
import Data.List
import Data.List.Split
import Data.Maybe (fromJust)
type Call = [String]
type Response = [String]

type Challenge = (Call, Response)

isSubset :: String -> String -> Bool
isSubset small large = all (`elem` large) small


solveChallenge :: Challenge -> [Int]
solveChallenge (call, response) =
    let find lambda = head $ filter lambda call
        one = find (\x -> length x == 2)
        seven = find (\x -> length x == 3)
        four = find (\x -> length x == 4)
        eight = find (\x -> length x == 7)
        six = find (\x -> (length x == 6) && not (isSubset one x))
        nine = find (\x -> (length x == 6) && isSubset four x)
        zero = find (\x -> (length x == 6) && (x /= nine) && (x /= six))
        three = find (\x -> (length x == 5) && isSubset one x)
        five = find (\x -> length x == 5 && isSubset x six)
        two = find (\x -> (length x == 5) && x /= three && x /= five)
        lookupKey =  [(one, 1), (two, 2), (three, 3), (four, 4), (five, 5),
                     (six, 6), (seven, 7), (eight, 8), (nine, 9), (zero, 0)]
    in map (fromJust . (`lookup` lookupKey)) response

digitsToInt :: Int -> [Int] -> Int
digitsToInt base = foldr (\x accum -> accum * base + x) 0 . reverse

parseChallenge :: String -> Challenge
parseChallenge rawChallenge =
    let (rawCall:rawResponse:_) = splitOn " | " rawChallenge
    in (map sort $ words rawCall, map sort $ words rawResponse)

readInput :: IO [Challenge]
readInput = map parseChallenge . lines <$> getContents

main = do
    allChallenges <- readInput
    let
        allResponses = map solveChallenge allChallenges
        easyCount = length $ filter (`elem` [1, 4, 7, 8]) $ concat allResponses
        sumResponses = sum $ map (digitsToInt 10) allResponses
    print (easyCount, sumResponses)
