module Day13 where
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import Data.List.Split (splitOn, chunksOf)
import Data.List
import Text.Printf (errorBadArgument, printf)
import Debug.Trace (traceShowId)
type Coordinate = (Int, Int)
data Fold = AlongX Int | AlongY Int
type Paper = S.Set Coordinate

flattenCoord :: Int -> Coordinate -> Int
flattenCoord width (x, y) = y * width + x

foldCoord :: Fold -> Coordinate -> Coordinate
foldCoord (AlongX foldX) (x, y) = (foldX - abs (foldX - x), y)
foldCoord (AlongY foldY) (x, y) = (x, foldY - abs (foldY - y))

applyFold :: Fold -> Paper -> Paper
applyFold fold = S.map (foldCoord fold)

showPaper :: Paper -> String
showPaper paper =
    let coords = traceShowId $ S.toList paper
        width = maximum (map fst coords) + 1
        height = maximum (map snd coords) + 1
        flatCoords = map (flattenCoord width) coords
        vector = V.replicate (width * height) '.'
        string = V.toList $ vector V.// zip flatCoords (repeat '#')
    in  intercalate "\n" $ chunksOf width string


parseFold :: String -> Fold
parseFold line
    | "fold along x=" `isPrefixOf` line = AlongX (read (drop 13 line) :: Int)
    | "fold along y=" `isPrefixOf` line = AlongY (read (drop 13 line) :: Int)
    | otherwise = errorWithoutStackTrace ("parse error: " ++ line)

parsePaper :: IO (Paper, [Fold])
parsePaper = do
    contents <- getContents
    let rawPoints:rawFolds:_ = splitOn "\n\n" contents
        parsedPoints = S.fromList $ map ((\(x:y:_)-> (read x, read y)) . splitOn ",") (lines rawPoints)
        parsedFolds = map parseFold (lines rawFolds)
    return (parsedPoints, parsedFolds)

main = do
    (paper, folds) <- parsePaper
    print $ S.size $ applyFold (head folds) paper
    printf $ showPaper $ foldl (flip applyFold) paper folds