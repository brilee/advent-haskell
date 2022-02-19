module Day15 where
import qualified AOC

import qualified Data.MemoCombinators as Memo
import qualified Data.Vector.Unboxed as V
import qualified Data.Heap as H
import Data.Char (digitToInt)
import qualified Data.Map.Internal as M
import Data.Maybe
import Debug.Trace

-- This is the wrong implementation! D'oh. Cave path can curl around, too...
-- getLowestRisk :: Cave -> Coord -> Int
-- getLowestRisk cave = innerf
--     where
--         innerf = Memo.pair Memo.integral Memo.integral innerf'
--             where
--                 innerf' (0, 0) = 0
--                 innerf' (0, y) = getCoordRisk cave (0, y) + innerf (0, y-1)
--                 innerf' (x, 0) = getCoordRisk cave (x, 0) + innerf (x-1, 0)
--                 innerf' (x, y) = getCoordRisk cave (x, y) +
--                     min (innerf (x-1, y)) (innerf (x, y-1))

expandFrontier :: AOC.Grid Int ->
    H.MinPrioHeap Int AOC.Coord ->
    M.Map AOC.Coord Int ->
    M.Map AOC.Coord Int
expandFrontier cave priorityQueue riskScores
    | H.isEmpty priorityQueue = riskScores
    | otherwise = let
        ((riskScore, nextCoord):_, remainingQueue) = H.splitAt 1 priorityQueue
        in if M.member nextCoord riskScores
            then expandFrontier cave remainingQueue riskScores
            else let
                neighbors = AOC.get4Neighbors cave nextCoord
                neighborCosts = map ((+riskScore) . AOC.getCoordValue cave) neighbors
                expandedQueue = H.union remainingQueue (H.fromList $ zip neighborCosts neighbors)
                expandedRiskScores = M.insert nextCoord riskScore riskScores
            in expandFrontier cave expandedQueue expandedRiskScores

minimumRisk :: AOC.Grid Int -> Int
minimumRisk cave = 
    let costs = expandFrontier cave (H.singleton (0, (0, 0))) M.empty 
        (sizeX, sizeY, _) = cave
    in fromJust $ M.lookup (sizeX - 1, sizeY - 1) costs

tileCave :: AOC.Grid Int -> AOC.Grid Int
tileCave (sizeX, sizeY, grid) =
    (5 * sizeX, 5 * sizeY, newGrid)
    where
        findValue flatCoord = 
            let (virtX, virtY) = divMod flatCoord (5 * sizeY)
                (offsetX, x) = divMod virtX sizeX
                (offsetY, y) = divMod virtY sizeY
                origVal = AOC.getCoordValue (sizeX, sizeY, grid) (x, y)
                newVal = origVal + offsetX + offsetY
            in if newVal > 9 then newVal - 9 else newVal
        newGrid = V.generate (25 * sizeX * sizeY) findValue 

main = do
    cave <- AOC.parseGrid
    print $ minimumRisk cave
    let tiledCave = tileCave cave
    print $ minimumRisk tiledCave