module Day12 where

import Data.List.Split
import qualified Data.Set as S
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.Functor
import Data.Char (isLower, isUpper)
import Debug.Trace (traceShowId)
import Data.List (sort, intercalate)
import Text.Printf (printf)

type Node = String
type Path = [Node]
type Cave = [(Node, [Node])] --adjacency list form

countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences elem list = length $ filter (==elem) list

hasVisitedSmallTwice :: Path -> Bool
hasVisitedSmallTwice path =
    let smalls = filter (all isLower) path
    in length smalls /= length (S.toList $ S.fromList smalls)

checkNotVisited :: [Node] -> Node -> Bool
checkNotVisited path node
  | all isUpper node = True
  | node == "start" = False
  | node == "end" = True
--  | otherwise = countOccurrences node path == 0
  | hasVisitedSmallTwice path = countOccurrences node path == 0
  | otherwise = countOccurrences node path <= 1

propagatePaths :: Cave -> [Path] -> [Path] -> [Path]
propagatePaths _ [] completePaths = completePaths
propagatePaths cave (currPath:remainingPaths) completePaths =
    if head currPath == "end"
        then propagatePaths cave remainingPaths (currPath:completePaths)
        else let
            possibleNext = fromJust $ lookup (head currPath) cave
            legalNext = filter (checkNotVisited currPath) possibleNext
            nextPaths = map (: currPath) legalNext
            in propagatePaths cave (nextPaths ++ remainingPaths) completePaths

searchCave :: Cave -> [Path]
searchCave cave = propagatePaths cave [["start"]] []

pair [a, b] = (a, b)

readCave :: IO Cave
readCave = do
    rawEdges <- lines <$> getContents
    let edges = map (pair . take 2 . splitOn "-") rawEdges
        directedEdges = edges ++ map swap edges
        nodes = S.toList $ S.fromList (map fst directedEdges)
        findAdjacencies node = (node, [y | (x, y) <- directedEdges, x == node])
    return $ map findAdjacencies nodes

main = readCave >>= print . length . searchCave
--main = readCave >>= printf . intercalate "\n" . map (intercalate ",") . sort . map reverse . searchCave