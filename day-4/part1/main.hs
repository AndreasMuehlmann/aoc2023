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

parseCardSet :: T.Text -> ([Int],  [Int])
parseCardSet cardSet = (parseNumbersToList (head splitCardSet), parseNumbersToList (last splitCardSet))
                     where splitCardSet = T.split (== '|') $ last $ T.split (== ':') cardSet

parseCardSets :: T.Text -> [([Int], [Int])]
parseCardSets = map parseCardSet . T.lines

solveCardSet :: ([Int], [Int]) -> Int
solveCardSet (correctCards, drawnCards) = if lengthDrawnCorrectCards == 0 then 0 else 2 ^ (lengthDrawnCorrectCards - 1)
    where lengthDrawnCorrectCards = length $ filter (`elem` correctCards) drawnCards

solve :: [([Int], [Int])] -> Int
solve = sum . map solveCardSet

main :: IO ()
main = do
    contents <- TIO.readFile "part1.txt"
    print $ solve $ parseCardSets contents