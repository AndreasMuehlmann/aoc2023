module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO


textToInt :: T.Text -> Int
textToInt text = case TR.decimal text of
    Left _err -> -1
    Right (result, _) -> result

parseNumbersToList :: [T.Text] -> [Int]
parseNumbersToList = map textToInt

parseLineWithIdentifier :: T.Text -> [Int]
parseLineWithIdentifier = parseNumbersToList . tail . T.words

parseRaces :: T.Text -> [(Int, Int)]
parseRaces contents = zip (parseLineWithIdentifier $ head $ T.lines contents) (parseLineWithIdentifier $ last $ T.lines contents)

solveRace :: (Int, Int) -> Int
solveRace (time, record) = ceiling ((fromIntegral (-time) - sqrt discriminatory) / (-2) - 1) - floor ((fromIntegral (-time) + sqrt discriminatory) / (-2) + 1) + 1
        where discriminatory = fromIntegral $ time^2 - 4 * record

solve :: [(Int, Int)] -> Int
solve races = product $ map solveRace races

main :: IO ()
main = do
    contents <- TIO.readFile "part1.txt"
    print $ solve $ parseRaces contents