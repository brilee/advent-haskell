module Day11 where
import qualified Data.Vector.Unboxed as V
import qualified Data.Set as S
import Debug.Trace (traceShowId, traceShow)
import Data.Char (digitToInt)
import Data.List (findIndex)
type Octogrid = (Int, V.Vector Int)
type Coordinates = (Int, Int)

allCoords size = [(x, y) | x <- [0..size-1], y <- [0..size-1]]

flattenCoords :: Int -> Coordinates -> Int
flattenCoords size (x, y) = x * size + y

getCoord :: Octogrid -> Coordinates -> Int
getCoord (size, mapData) coord =
    mapData V.! flattenCoords size coord

getNeighbors :: Octogrid -> Coordinates -> [Coordinates]
getNeighbors (size, _) (x, y) =
    let possibleNeighbors = [(i, j) | i <-[x-1..x+1], j <- [y-1..y+1]]
        inBounds = (\(x, y) -> x `mod` size == x && y `mod` size == y)
    in filter inBounds possibleNeighbors

addFlash :: Octogrid -> Coordinates -> Octogrid
addFlash octogrid coord =
    let (size, mapData) = octogrid
        flatCoordsToIncrement = map (flattenCoords size) (getNeighbors octogrid coord)
    in (size, V.accum (+) mapData (zip flatCoordsToIncrement (repeat 1)))


searchFlashes :: Octogrid -> S.Set Coordinates -> [Coordinates] -> (Octogrid, S.Set Coordinates)
searchFlashes octogrid flashed [] = (octogrid, flashed)
searchFlashes octogrid flashed (nextCoord:remainingCoords) =
    if getCoord octogrid nextCoord < 10 || S.member nextCoord flashed
    then searchFlashes octogrid flashed remainingCoords
    else let newOctogrid = addFlash octogrid nextCoord
             newFlashed = S.insert nextCoord flashed
             newQueue = getNeighbors octogrid nextCoord ++ remainingCoords
         in searchFlashes newOctogrid newFlashed newQueue

resetOctogrid :: Octogrid -> S.Set Coordinates -> Octogrid
resetOctogrid (size, mapData) coords =
    let flatCoordsToReset = map (flattenCoords size) (S.toList coords)
    in (size, mapData V.// zip flatCoordsToReset (repeat 0))

stepGrid :: Octogrid -> (Octogrid, Int)
stepGrid (size, mapData) =
    let steppedGrid = V.map (+1) mapData
        (postFlashOctogrid, flashed) = searchFlashes (size, steppedGrid) S.empty (allCoords size)
        newOctogrid = resetOctogrid postFlashOctogrid flashed
        flashCount = length flashed
    in (newOctogrid, flashCount)

readOctogrid :: IO Octogrid
readOctogrid = do
    rawGrid <- lines <$> getContents
    let mapData = map digitToInt (concat rawGrid)
    return (length rawGrid, V.fromList mapData)

accumFlashes (octogrid, accum) =
        let (steppedGrid, flashCount) = stepGrid octogrid
        in (steppedGrid, flashCount + accum)

didAllFlash ((size, _), flashCount) = size * size == flashCount

main = do
    octogrid <- readOctogrid
    let flashSequence = iterate accumFlashes (octogrid, 0)
        (_, part1result) = flashSequence !! 100
    print part1result

    let gridSequence = iterate (stepGrid. fst) (octogrid, 0)
    print $ findIndex didAllFlash gridSequence
