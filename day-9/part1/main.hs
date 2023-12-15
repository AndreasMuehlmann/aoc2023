module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO


textToNat :: T.Text -> Int
textToNat text = case TR.decimal text of
    Left _err -> -1
    Right (result, _) -> result

textToInt :: T.Text -> Int
textToInt text
    | T.head text == '-' = -1 * textToNat (T.tail text)
    | otherwise = textToNat text

parseNumbersToList :: T.Text -> [Int]
parseNumbersToList = map textToInt . T.words

parseOasisData :: T.Text -> [[Int]]
parseOasisData = map parseNumbersToList . T.lines

differences :: Num a => [a] -> [a]
differences xs = zipWith (\ x y -> y - x) xs (tail xs)

solveSeries :: [Int] -> Int
solveSeries series
    | all (== 0) series = 0
    | otherwise = last series + solveSeries (differences series)

solve :: [[Int]] -> Int
solve = sum . map solveSeries

main :: IO ()
main = do
    contents <- TIO.readFile "part1.txt"
    print $ solve $ parseOasisData contents