module Day2a where
data Direction = Forward | Down | Up
data Location = Location Int Int

lineParser :: String -> (Direction, Int)
lineParser line = (dir, distance)
    where (dirString:distanceString:_) = words line
          distance = read distanceString
          dir = case dirString of "forward" -> Forward
                                  "up" -> Up
                                  "down" -> Down
                                  _ -> Down
                                  
directionReader :: IO [(Direction, Int)]
directionReader = map lineParser . lines <$> getContents

dive :: Location -> (Direction, Int) -> Location
dive (Location x y) (Forward, distance) = Location (x + distance) y
dive (Location x y) (Down, distance) = Location x (y + distance)
dive (Location x y) (Up, distance) = Location x (y - distance)

main = do
    directionStream <- directionReader
    let (Location finalPos finalDepth) = foldl dive (Location 0 0) directionStream
    print $ finalPos * finalDepth
