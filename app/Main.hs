import qualified Day1
import qualified Day2a
import qualified Day2b
import qualified Day3a
import qualified Day3b
import qualified Day4a
import qualified Day4b
import qualified Day5a
import qualified Day6
import qualified Day7a
import qualified Day7b
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day14b
import qualified Day15
import System.Environment (getArgs)
import Data.Maybe (fromJust)


dispatch = [
    ("Day1", Day1.main),
    ("Day2a", Day2a.main),
    ("Day2b", Day2b.main),
    ("Day3a", Day3a.main),
    ("Day3b", Day3b.main),
    ("Day4a", Day4a.main),
    ("Day4b", Day4b.main),
    ("Day5a", Day5a.main),
    ("Day6", Day6.main),
    ("Day7a", Day7a.main),
    ("Day7b", Day7b.main),
    ("Day8", Day8.main),
    ("Day9", Day9.main),
    ("Day10", Day10.main),
    ("Day11", Day11.main),
    ("Day12", Day12.main),
    ("Day13", Day13.main),
    ("Day14", Day14.main),
    ("Day14b", Day14b.main),
    ("Day15", Day15.main)]

main = do
    args <- getArgs
    let day = head args
        mainFn = fromJust $ lookup day dispatch
    mainFn