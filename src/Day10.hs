module Day10 where
import Data.Maybe (fromJust)
import Data.List (foldl', sort)
import Debug.Trace (traceShowId)
data MaybeStack = Stack String | Error Char deriving Show

checkMatch :: Char -> Char -> Bool
checkMatch c1 c2
    | c1 == '{' && c2 == '}' = True
    | c1 == '(' && c2 == ')' = True
    | c1 == '[' && c2 == ']' = True
    | c1 == '<' && c2 == '>' = True
    | otherwise = False

handleChar :: MaybeStack -> Char -> MaybeStack
handleChar (Error c) _ = Error c
handleChar (Stack "") c
    | c `elem` "{([<" = Stack [c]
    | c `elem` "})]>" = Error c
handleChar (Stack (top:rest)) c
    | c `elem` "{([<" = Stack $ c:top:rest
    | c `elem` "})]>" && checkMatch top c = Stack rest
    | c `elem` "})]>" && not (checkMatch top c) = Error c

scoreCorrupted :: String -> Int
scoreCorrupted rawInput =
    let finishedStack = foldl' handleChar (Stack "") rawInput
    in case finishedStack of
        (Error c) -> fromJust $ lookup c [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
        _ -> 0

digitsToIntBase5 :: [Int] -> Int
digitsToIntBase5 = foldr (\x accum -> accum * 5 + x) 0 . reverse

scoreIncomplete :: String -> Int
scoreIncomplete rawInput =
    let finishedStack = foldl' handleChar (Stack "") rawInput
    in case finishedStack of
        (Error c) -> 0
        (Stack s) -> digitsToIntBase5 $ map (fromJust . (\x -> lookup x [('(', 1), ('[', 2), ('{', 3), ('<', 4)])) s

median :: [Int] -> Int
median list = list !! (length list `div` 2)

main = do
    chunks <- lines <$> getContents
    print $ sum $ map scoreCorrupted chunks -- ["{([(<{}[<>[]}>{[]{[(<()>"]
    print $ median . sort . filter (>0) $ map scoreIncomplete chunks