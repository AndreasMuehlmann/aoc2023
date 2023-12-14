module Main where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO

import Data.List ( sortBy, group, sort)
import Data.Char ( digitToInt, isDigit )


textToInt :: T.Text -> Int
textToInt text = case TR.decimal text of
    Left _err -> -1
    Right (result, _) -> result

parseCardSet :: T.Text -> (T.Text, Int)        
parseCardSet line = (head wordsLine, textToInt $ last wordsLine)
             where wordsLine = T.words line
    
parseCardSets :: T.Text -> [(T.Text, Int)]
parseCardSets = map parseCardSet . T.lines

valueCard :: Char -> Int
valueCard card
        | isDigit card = digitToInt card
        | card == 'T' = 10
        | card == 'J' = 1
        | card == 'Q' = 11
        | card == 'K' = 12
        | card == 'A' = 13

totalValueCards :: String -> Int
totalValueCards [card] = valueCard card
totalValueCards (card:cards) = valueCard card * (14 ^ length (card:cards)) + totalValueCards cards

typeCardSet :: String -> Int
typeCardSet cards
        | jokers == 5 = 6
        | length groupedCards == 1 = 6
        | length groupedCards == 2 && maximum lengthGroupedCards == 4 - jokers = 5
        | length groupedCards == 2 = 4
        | length groupedCards == 3 && maximum lengthGroupedCards == 3 - jokers = 3
        | length groupedCards == 3 = 2
        | length groupedCards == 4 = 1
        | otherwise = 0
        where jokers = length $ filter (== 'J') cards
              filteredCards = filter (/= 'J') cards
              groupedCards = group $ sort filteredCards
              lengthGroupedCards = map length groupedCards
              

isFirstStronger :: T.Text -> T.Text -> Bool
isFirstStronger cards1 cards2
        | typeCards1 > typeCards2 = True
        | typeCards1 < typeCards2 = False
        | otherwise = totalValueCards (T.unpack cards1) > totalValueCards (T.unpack cards2)
        where typeCards1 = typeCardSet $ T.unpack cards1
              typeCards2 = typeCardSet $ T.unpack cards2

compareCardSets :: (T.Text, Int) -> (T.Text, Int) -> Ordering
compareCardSets (cards1, bid1) (cards2, bid2)
        | cards1 == cards2 = EQ
        | isFirstStronger cards1 cards2 = GT
        | otherwise = LT

solve :: [(T.Text, Int)] -> Int
solve cardSets = sum $ zipWith (*) [1..] $ map snd $ sortBy compareCardSets cardSets

main :: IO ()
main = do
    contents <- TIO.readFile "part2.txt"
    print $ solve $ parseCardSets contents