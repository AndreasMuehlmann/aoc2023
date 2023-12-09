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

scoreLengthWinningCards :: Int -> Int
scoreLengthWinningCards scoreLengthWinningCards = if scoreLengthWinningCards == 0 then 0 else 2 ^ (scoreLengthWinningCards - 1)

solveCardSet :: ([Int], [Int]) -> Int
solveCardSet (correctCards, drawnCards) = length $ filter (`elem` correctCards) drawnCards

actualValuesCardsHelper :: [Int] -> [Int] -> [Int]
actualValuesCardsHelper = foldl
      (\ previousActualValues cardCorrectCount
         -> 1 + sum (take cardCorrectCount previousActualValues)
              : previousActualValues)

actualValuesCards :: [Int] -> [Int]
actualValuesCards = actualValuesCardsHelper [] . reverse

solve :: [([Int], [Int])] -> Int
solve cardSets = sum $ actualValuesCards correctCardsperSet
                 where correctCardsperSet = map solveCardSet cardSets

main :: IO ()
main = do
    contents <- TIO.readFile "part2.txt"
    print $ solve $ parseCardSets contents