module Day5a where
import Data.List
import Data.List.Split
import Control.Monad
import qualified Data.Vector.Unboxed as V
import Debug.Trace (traceShowId)

type VentLine = (Int, Int, Int, Int)
type VentMap = V.Vector Int

mapSize = 1000

newVent = V.replicate (mapSize * mapSize) 0 :: V.Vector Int

isVentHorizontal :: Eq a => (a, b, a, d) -> Bool
isVentHorizontal (x1, y1, x2, y2) = x1 == x2
isVentVertical (x1, y1, x2, y2) = y1 == y2
isVentDiagonal (x1, y1, x2, y2) = abs (x1 - x2) == abs (y1 - y2)

makeRange :: Int -> Int -> V.Vector Int
makeRange x1 x2
    | x1 > x2 = V.enumFromStepN x1 (-1) (x1 - x2 + 1)
    | otherwise = V.enumFromN x1 (x2 - x1 + 1)

mapWithLine :: VentLine -> VentMap
mapWithLine vent =
    let (x1, y1, x2, y2) = vent
        coords
          | isVentVertical vent =
               V.map (`p2i` y1) (makeRange x1 x2)
          | isVentHorizontal vent =
               V.map (p2i x1) (makeRange y1 y2)
          | isVentDiagonal vent =
               V.zipWith p2i (makeRange x1 x2) (makeRange y1 y2)
          | otherwise = V.empty
    in V.update_ newVent coords (V.replicate (V.length coords) 1)

addVent :: VentLine -> VentMap -> VentMap
addVent line ventMap =
    let mapToAdd = mapWithLine line
    in V.zipWith (+) mapToAdd ventMap

p2i :: Int -> Int -> Int
i2p :: Int -> (Int, Int)
p2i x y = mapSize * x + y
i2p index = divMod index mapSize

parseVentLine :: String -> VentLine
parseVentLine line =
    let (start:end:_) = splitOn " -> " line
        (x1:y1:_) = map read $ splitOn "," start
        (x2:y2:_) = map read $ splitOn "," end
    in (x1, y1, x2, y2)

main = do
    ventLines <- map parseVentLine . lines <$> getContents :: IO [VentLine]
    let completedMap = foldl (flip addVent) newVent ventLines
        hasVentCover = V.map (\x -> if x > 1 then 1 else 0) completedMap :: V.Vector Int
    print $ V.sum hasVentCover
