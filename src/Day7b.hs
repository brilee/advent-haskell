module Day7b where
import Data.List
import Data.List.Split
import Debug.Trace (traceShowId)

distance :: Num a => a -> a -> a
distance a b = abs (a - b)
average a b = (a + b) `div` 2
triangle a = (a * (a + 1)) `div` 2

ternarySearchMax :: (Integral a, Ord b) => (a -> b) -> a -> a -> b
ternarySearchMax f lo hi
    | (hi - lo) < 2 = max (f lo) (f hi)
    | otherwise = ternaryHelper f (lo, average lo hi, hi) (f lo, f (average lo hi), f hi)

ternaryHelper :: (Integral a, Ord b) => (a -> b) -> (a, a, a) -> (b, b, b) -> b
ternaryHelper f (lo, mid, hi) (loVal, midVal, hiVal)
    | lo + 1 == mid && mid == hi - 1 = maximum [loVal, midVal, hiVal]
    | distance lo mid > distance mid hi = let
        leftMid = average lo mid
        leftMidVal = f leftMid
        in if leftMidVal > midVal
            then ternaryHelper f (lo, leftMid, mid) (loVal, leftMidVal, midVal)
            else ternaryHelper f (leftMid, mid, hi) (leftMidVal, midVal, hiVal)
    | otherwise = let
        rightMid = average mid hi
        rightMidVal = f rightMid
        in if rightMidVal > midVal
            then ternaryHelper f (mid, rightMid, hi) (midVal, rightMidVal, hiVal)
            else ternaryHelper f (lo, mid, rightMid) (loVal, midVal, rightMidVal)

crabCost :: [Int] -> Int -> Int
crabCost crabs center = - (sum $ map (triangle . distance center) crabs)

parseCrabs :: IO [Int]
parseCrabs = map read . splitOn "," <$> getLine
main = do
    crabs <- parseCrabs
    let minCrabCost = ternarySearchMax (crabCost crabs) 0 (maximum crabs)
    print (- minCrabCost)
