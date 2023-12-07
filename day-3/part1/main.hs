module Main where

import Data.Char


data NumberPos = NumberPosCons Int Int Int deriving Show


getNumberHelper :: String -> Int -> Int -> (Int, Int)
getNumberHelper (char:chars) number len
                    | isDigit char = getNumberHelper chars (number * 10 + digitToInt char) (len + 1)
                    | otherwise = (number, len)

getNumber :: String -> (Int, Int)
getNumber string = getNumberHelper string 0 0

parseNumbers :: String -> [NumberPos] -> Int -> Int-> [NumberPos]
parseNumbers (char:chars) accumulator x y 
        | null chars = if isDigit char then NumberPosCons number x y : accumulator else accumulator
        | isDigit char = parseNumbers (drop (len - 1) chars) digitCell (x + 1) y
        | char == '\n' = parseNumbers chars accumulator 0 (y + 1)
        | otherwise = parseNumbers chars accumulator (x + 1) y
        where (number, len) = getNumber (char:chars)
              digitCell = NumberPosCons number x y : accumulator

main :: IO ()
main = do
    contents <- readFile "test_part1.txt"
    print $ parseNumbers contents [] 0 0

