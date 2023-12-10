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

parseSeeds :: T.Text -> [Int]
parseSeeds = parseNumbersToList . T.unwords . tail . T.words

parseAlmanac :: T.Text -> ([Int], [[[Int]]])
parseAlmanac text = (parseSeeds $ head linesText, map parseAlmanacMap $ splitIntoAlmanacMaps linesText)
        where linesText = T.lines text

inRangeOfRule :: Int -> [Int] -> Bool
inRangeOfRule num rule = rule!!1 <= num && num < rule!!1 + last rule

applyRule :: Int -> [Int] -> Int
applyRule num rule = num - rule!!1 + head rule

mapByAlamanacMap :: [[Int]] -> Int -> Int
mapByAlamanacMap alamanacMap num
        | null fittingRules = num 
        | otherwise = applyRule num $ head fittingRules
        where fittingRules = filter (inRangeOfRule num) alamanacMap

convertByMaps :: ([Int], [[[Int]]]) -> [Int]
convertByMaps (seeds, maps)
        | null maps = seeds
        | otherwise = convertByMaps (map (mapByAlamanacMap (head maps)) seeds, tail maps)

solve :: ([Int], [[[Int]]]) -> Int
solve (seeds, maps) = minimum $ convertByMaps (seeds, maps)

main :: IO ()
main = do
    contents <- TIO.readFile "test_part2.txt"
    print $ solve $ parseAlmanac contents