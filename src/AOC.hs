module AOC where
import qualified Data.Vector.Unboxed as V
import Data.Char

-- 2d grid helpers
type Coord = (Int, Int)
type Grid a = (Int, Int, V.Vector a)

makeGrid :: V.Unbox a => Int -> Int -> a -> Grid a
makeGrid sizeX sizeY initVal = (sizeX, sizeY, V.replicate (sizeX * sizeY) initVal)

flattenCoord :: V.Unbox a => Grid a -> Coord -> Int
flattenCoord (sizeX, sizeY, _) (x, y) = x * sizeY + y

getCoordValue :: V.Unbox a => Grid a -> Coord -> a
getCoordValue grid coord =
    let (_, _, gridData) = grid
    in gridData V.! flattenCoord grid coord

get4Neighbors :: Grid a -> Coord -> [Coord]
get4Neighbors (sizeX, sizeY, _) (x, y) =
    let possibleNeighbors = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
        inBounds = (\(x, y) -> x `mod` sizeX == x && y `mod` sizeY == y)
    in filter inBounds possibleNeighbors

get8Neighbors :: Grid a -> Coord -> [Coord]
get8Neighbors (sizeX, sizeY, _) (x, y) =
    let possibleNeighbors = [(i, j) | i <-[x-1..x+1], j <- [y-1..y+1]]
        inBounds = (\(x, y) -> x `mod` sizeX == x && y `mod` sizeY == y)
    in filter inBounds possibleNeighbors

parseGrid :: IO (Grid Int)
parseGrid = do
    rows <- lines <$> getContents
    return (length $ head rows, length rows, V.fromList $ concatMap (map digitToInt) rows)

-- numeric/string helpers
digitsToInt :: Int -> [Int] -> Int
digitsToInt base = foldr (\x accum -> accum * base + x) 0 . reverse
