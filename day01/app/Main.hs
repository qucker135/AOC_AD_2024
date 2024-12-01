module Main where
import qualified Data.List (sort)

firstTwoEls :: [a] -> (a, a)
firstTwoEls (x1 : x2 : _) = (x1, x2)

sortTupleOfLists :: Ord a => Ord b => ([a], [b]) -> ([a], [b])
sortTupleOfLists (list1, list2) = (Data.List.sort list1, Data.List.sort list2)

zipTuple :: ([a], [b]) -> [(a, b)]
zipTuple (list1, list2) = zip list1 list2

absDiff :: Num a => Ord a => (a, a) -> a
absDiff (x, y) = abs (x - y)

nrOfOccs :: Integer -> [Integer] -> Integer
nrOfOccs x = toInteger . length . filter (== x)

sumOfnrOfOccsInTuple :: ([Integer], [Integer]) -> Integer
sumOfnrOfOccsInTuple (list1, list2) = sum $ map (\x -> (x * (nrOfOccs x list2))) list1

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ sumOfnrOfOccsInTuple $ unzip $ map (firstTwoEls . (map (read :: String -> Integer)) . words) $ lines contents
    