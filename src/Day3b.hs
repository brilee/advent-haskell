module Day3b where
type Entry = [Int]

zeroEntry = replicate 12 0

addEntry :: Entry -> Entry -> Entry
addEntry = zipWith (+)

addEntries :: [Entry] -> Entry
addEntries =  foldl addEntry zeroEntry

findMajority :: [Entry] -> Entry
findMajority entries =
    let entrySum = addEntries entries
        numEntries = length entries
    in map (\e -> if e * 2 >= numEntries then 1 else 0) entrySum

findMinority :: [Entry] ->  Entry
findMinority entries =
    let entrySum = addEntries entries
        numEntries = length entries
    in map (\e -> if e * 2 < numEntries then 1 else 0) entrySum

findAgreement :: Int -> ([Entry] -> Entry) -> [Entry] -> [Entry]
findAgreement index agreeFn entries =
    let referenceEntry = agreeFn entries
    in filter (\e -> e !! index == referenceEntry !! index) entries

reduceAgreement :: ([Entry] -> Entry) -> [Entry] -> Entry
reduceAgreement agreeFn entries =
    f 0 entries
    where f _ [entry] = entry
          f i entries = f (i + 1) (findAgreement i agreeFn entries)

parseDigits :: Int -> Entry -> Int
parseDigits base = parseFlippedDigits base . reverse

parseFlippedDigits :: Int -> Entry -> Int
parseFlippedDigits base [] = 0
parseFlippedDigits base (x:xs) = x + base * parseFlippedDigits base xs

lineParser :: String -> Entry
lineParser = map (\c -> if c == '1' then 1 else 0)
entryReader :: IO [Entry]
entryReader = map lineParser . lines <$> getContents

main = do
    entries <- entryReader
    let oxy = parseDigits 2 (reduceAgreement findMajority entries) 
        co2 = parseDigits 2 (reduceAgreement findMinority entries)
    print $ oxy * co2
