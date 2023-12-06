module Main where

import Data.Char


data DigitCell = DigitCellCons Int Int Int 
data CharCell = CharCellCons Char Int Int 

instance Show DigitCell where
   show (DigitCellCons digit x y) = show [digit, x, y]

solve :: [DigitCell] -> [CharCell] -> Int
solve cells text = 0

parseDigits :: String -> [DigitCell] -> Int -> Int-> [DigitCell]
parseDigits (char:chars) cellsAccumulator x y 
        | null chars = if isDigit char then DigitCellCons (digitToInt char) x y : cellsAccumulator else cellsAccumulator
        | isDigit char = parseDigits chars (DigitCellCons (digitToInt char) x y : cellsAccumulator) (x + 1) y
        | char == '\n' = parseDigits chars cellsAccumulator 0 (y + 1)
        | otherwise = parseDigits chars cellsAccumulator (x + 1) y

main :: IO ()
main = do
    contents <- readFile "test_part1.txt"
    print $ solve (parseDigits contents [] 0 0) []