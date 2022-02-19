module Day2b where

data Direction = Forward | Down | Up
data Location = Location {x, y, angle :: Int}

lineParser :: String -> (Direction, Int)
lineParser line = (dir, val)
    where (dirString:distanceString:_) = words line
          val = read distanceString
          dir = case dirString of "forward" -> Forward
                                  "up" -> Up
                                  "down" -> Down
                                  _ -> Down

directionReader :: IO [(Direction, Int)]
directionReader = map lineParser . lines <$> getContents

dive :: Location -> (Direction, Int) -> Location
dive (Location x y angle) (Forward, val) = Location (x + val) (y + val * angle) angle
dive (Location x y angle) (Down, val) = Location x y (angle + val)
dive (Location x y angle) (Up, val) = Location x y (angle - val)

main = do
    directionStream <- directionReader
    let (Location finalPos finalDepth _) = foldl dive (Location 0 0 0) directionStream
    print $ finalPos * finalDepth
