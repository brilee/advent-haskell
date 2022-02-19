module Day1 where
intReader :: IO [Int]
intReader = map read . lines <$> getContents

intParser :: IO String -> IO [Int]
intParser s = map read . lines <$> s

main = do
    intStream <- intReader
    let pairStream = zip intStream (tail $ tail $ tail intStream)
    print $ sum [1 | (x, y) <- pairStream, x < y]
