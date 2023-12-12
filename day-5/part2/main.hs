module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO

textToInt :: T.Text -> Int
textToInt text = case TR.decimal text of
    Left _err -> -1
    Right (result, _) -> result

parseNumbersToList :: T.Text -> [Int]
parseNumbersToList = map textToInt . T.words

parseAlmanacMap :: T.Text -> [[Int]]
parseAlmanacMap = map parseNumbersToList . tail . T.lines

splitIntoAlmanacMaps :: [T.Text] -> [T.Text]
splitIntoAlmanacMaps = T.splitOn (T.pack "\n\n") . T.unlines . tail . tail

pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs [element] = []
pairs [element1, element2] = [(element1, element2)]
pairs (element1:element2:elements) = (element1, element2) : pairs elements

parseSeeds :: T.Text -> [(Int, Int)]
parseSeeds = pairs . parseNumbersToList . T.unwords . tail . T.words

parseAlmanac :: T.Text -> ([(Int, Int)], [[[Int]]])
parseAlmanac text = (parseSeeds $ head linesText, map parseAlmanacMap $ splitIntoAlmanacMaps linesText)
        where linesText = T.lines text

offsetByRule :: Int -> [Int] -> Int
offsetByRule num rule = num - rule!!1 + head rule

applyRule :: [Int] -> (Int, Int) -> [(Int, Int)]
applyRule rule (firstNum, range)
        | firstInRule && lastInRule= [(offsetByRule firstNum rule, range)]
        | firstInRule = [(offsetByRule firstNum rule, lastNumRule - firstNum + 1), (offsetByRule firstNum rule + lastNumRule - firstNum, range - rule!!1 + last rule - 1)]
        | lastInRule = [(0, 0)]
        | otherwise = [(0, 0)]
        where lastNum = firstNum + range - 1
              firstNumRule = rule!!1
              lastNumRule = firstNumRule + last rule - 1
              firstInRule = firstNumRule <= firstNum && firstNum <= lastNumRule
              lastInRule = firstNumRule <= lastNum && lastNum <= lastNumRule

mapByAlamanacMap :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
mapByAlamanacMap [rule] seedRanges = concatMap (applyRule rule) seedRanges
mapByAlamanacMap (rule:rules) seedRanges = mapByAlamanacMap rules $ concatMap (applyRule rule) seedRanges

convertByMaps :: ([(Int, Int)], [[[Int]]]) -> [(Int, Int)]
convertByMaps (seedRanges, maps)
        | null maps = seedRanges
        | otherwise = [(0, 0)] -- convertByMaps (map (mapByAlamanacMap (head maps)) seeds, tail maps)

solve :: ([(Int, Int)], [[[Int]]]) -> Int
solve (seeds, maps) = minimum $ map fst $ convertByMaps (seeds, maps)

main :: IO ()
main = do
    contents <- TIO.readFile "test_part2.txt"
    print $ solve $ parseAlmanac contents