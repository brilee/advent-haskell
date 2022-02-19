module Day9 where
import qualified Data.Vector.Unboxed as V
import Debug.Trace (traceShowId, traceShow)
import Data.List (sortOn)
import Data.Ord
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import qualified Data.Set as S
type LavaMap = (Int, Int, V.Vector Int)
type Coordinates = (Int, Int)

allCoords :: LavaMap -> [Coordinates]
allCoords (sizeX, sizeY, _) = [(x, y) | x <- [0..sizeX-1], y <- [0..sizeY-1]]

getCoord :: LavaMap -> Coordinates -> Int
getCoord (sizeX, sizeY, mapData) (x, y) =
    let flatCoord = x * sizeY + y
    in mapData V.! flatCoord

getNeighbors :: LavaMap -> Coordinates -> [Coordinates]
getNeighbors (sizeX, sizeY, _) (x, y) =
    let possibleNeighbors = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
        inBounds = (\(x, y) -> x `mod` sizeX == x && y `mod` sizeY == y)
    in filter inBounds possibleNeighbors

isLocalMinima :: LavaMap -> Coordinates -> Bool
isLocalMinima lavaMap coords =
    let neighbors = getNeighbors lavaMap coords
    in all ((> getCoord lavaMap coords) . getCoord lavaMap) neighbors

floodFillHelper :: LavaMap -> S.Set Coordinates -> S.Set Coordinates -> S.Set Coordinates
floodFillHelper lavaMap frontier floodResult
    | S.null frontier = floodResult
    | otherwise =
        let (nextCoord, remainingFrontier) = fromJust $ S.maxView frontier
        in if getCoord lavaMap nextCoord == 9
           then floodFillHelper lavaMap remainingFrontier floodResult
           else let neighbors = S.fromList (getNeighbors lavaMap nextCoord)
                    unexplored = S.filter (`S.notMember` floodResult) neighbors
                    newFrontier = S.union remainingFrontier unexplored
                    newFloodResult = S.insert nextCoord floodResult
                in floodFillHelper lavaMap newFrontier newFloodResult

floodFill :: LavaMap -> Coordinates -> S.Set Coordinates
floodFill lavaMap seedpoint = floodFillHelper lavaMap (S.singleton seedpoint) S.empty

findBasins :: LavaMap -> [S.Set Coordinates]
findBasins lavaMap =
    let potentialBasins = filter (\x -> getCoord lavaMap x /= 9) (allCoords lavaMap)
    in helper (S.fromList potentialBasins) []
    where
        helper remaining basins =
            if S.null remaining then basins else
            let floodResult = floodFill lavaMap (S.findMin remaining)
                unexplored = S.difference remaining floodResult
            in helper unexplored basins ++ [floodResult]

readLavaMap :: IO LavaMap
readLavaMap = do
    lavaLines <- lines <$> getContents
    let mapData = map digitToInt (concat lavaLines)
    return (length lavaLines, length $ head lavaLines, V.fromList mapData)

main = do
    lavaMap <- readLavaMap
    let minimumCoords = filter (isLocalMinima lavaMap) (allCoords lavaMap)
        riskLevels = map (getCoord lavaMap) minimumCoords
    print $ sum (map (+1) riskLevels)
    let basins = findBasins lavaMap
        basinSizes = map S.size basins
        sortedBasinSizes = sortOn Down basinSizes
        largestThreeBasins = take 3 sortedBasinSizes
    print $ product largestThreeBasins

